(ns korb.core
  (:require [clojure.core.async :as async :refer [<!!]])
  (:import (krpc.client Connection)
           (krpc.client.services SpaceCenter KRPC KRPC$Expression UI UI$MessagePosition)
           (org.javatuples Triplet)
           (java.lang Math)))

;; Orbital math.
(defn orbit-circularization-cost
  "Calculates the delta/v required to circularize the given orbit using the vis-viva equation.

   https://en.wikipedia.org/wiki/Vis-viva_equation
   "
  [orbit]
  (let [mu (-> orbit .getBody .getGravitationalParameter)
        r (.getApoapsis orbit)
        a1 (.getSemiMajorAxis orbit)
        v1 (Math/sqrt (* mu (- (/ 2.0 r) (/ 1.0 a1))))
        v2 (Math/sqrt (* mu (- (/ 2.0 r) (/ 1.0 r))))]
    (- v2 v1)))

(defn burn-time
  "Calculates the length of the burn required for the vessel to achieve the given delta-v using the rocket equation.

  https://en.wikipedia.org/wiki/Tsiolkovsky_rocket_equation"
  [vessel delta-v]
  (let [force (.getAvailableThrust vessel)
        isp (* (.getSpecificImpulse vessel) (-> vessel .getOrbit .getBody .getSurfaceGravity))
        m0 (.getMass vessel)
        m1 (/ m0 (Math/exp (/ delta-v isp)))
        flow-rate (/ force isp)]
    (/ (- m0 m1) flow-rate)))

;; Kerbal Space Program interface functions.
(defn show-message [conn msg] (-> conn UI/newInstance (.message msg 1.0 UI$MessagePosition/TOP_CENTER (Triplet. 255.0 255.0 0.0) 20.0)))

(defn burn [conn duration]
  (let [ctrl (-> conn SpaceCenter/newInstance .getActiveVessel .getControl)]
    (.setThrottle ctrl 1.0)
    (Thread/sleep (* 1000 duration))
    (.setThrottle ctrl 0.0)))

;; Basic maneuvers

(defn engage-autopilot [] #(-> % SpaceCenter/newInstance .getActiveVessel .getAutoPilot .engage))
(defn disengage-autopilot [] #(-> % SpaceCenter/newInstance .getActiveVessel .getAutoPilot .disengage))
(defn engage-sas [] #(-> % SpaceCenter/newInstance .getActiveVessel .getControl (.setSas true)))
(defn disengage-sas [] #(-> % SpaceCenter/newInstance .getActiveVessel .getControl (.setSas false)))
(defn orient [pitch heading] #(-> % SpaceCenter/newInstance .getActiveVessel .getAutoPilot (.targetPitchAndHeading pitch heading)))
(defn throttle [pct] #(-> % SpaceCenter/newInstance .getActiveVessel .getControl (.setThrottle pct)))
(defn stage [] #(-> % SpaceCenter/newInstance .getActiveVessel .getControl .activateNextStage))
(defn serial [maneuvers] (fn [conn] (run! #(% conn) maneuvers)))
(defn parallel [maneuvers] (fn [conn]
                             (->> maneuvers
                                  (map (fn [m] (async/thread (m conn))))
                                  (async/merge)
                                  (async/reduce (constantly nil) nil)
                                  (<!!))))
(defn sleep [sec] (fn [_] (Thread/sleep (* 1000 sec))))
(defn message [msg] #(show-message % msg))

(defmacro maneuver [& body]
  `(serial [~@body]))

(defmacro parallel-maneuver [& body]
  `(parallel [~@body]))

;; More complex maneuvers
(defn wait-for
  "Constructs a maneuver that waits for a KSP server-side condition to become true.
   
   kRPC allows for the construction of expression trees that describe a condition. These trees can be passed to kRPC and waited upon.
   This function expects condition-func to construct the expression tree, given a connection. This function creates the event and waits on it."
  [condition-func] (fn [conn]
                     (let [event (-> conn KRPC/newInstance (.addEvent (condition-func conn)))]
                       (locking (.getCondition event)
                         (.waitFor event)))))

(defn wait-for-fuel-level
  "Waits until the given fuel type reaches the given level."
  [type level]
  (let [cond-func (fn [conn]
                    (let [vessel (-> conn SpaceCenter/newInstance .getActiveVessel)
                          ; Important note about the stage parameter here:
                          ; kRPC distinguishes between "stages" and "decouple stages". A "decouple stage" of a part is the stage in which
                          ; that part is decoupled from the rest of the vessel. In the case of boosters, it's often the stage that is
                          ; immediately following the current stage (hence the dec).
                          resources (.resourcesInDecoupleStage vessel (dec (-> vessel .getControl .getCurrentStage)) false)
                          call (.getCall conn resources "amount" (into-array Object [type]))]
                      (KRPC$Expression/lessThanOrEqual conn (KRPC$Expression/call conn call) (KRPC$Expression/constantFloat conn level))))]
    (wait-for cond-func)))

(defn wait-for-atmospheric-density
  "Waits until the atmospheric density around the vessel reaches the given density."
  [density]
  (let [cond-func (fn [conn]
                    (let [vessel (-> conn SpaceCenter/newInstance .getActiveVessel)
                          call (.getCall conn (.flight vessel (.getReferenceFrame vessel)) "getAtmosphereDensity" (into-array Object '()))]
                      (KRPC$Expression/lessThanOrEqual conn (KRPC$Expression/call conn call) (KRPC$Expression/constantFloat conn density))))]
    (wait-for cond-func)))

(defn wait-for-apoapsis
  "Waits until the apoapsis of the current orbit reaches the given altitude."
  [altitude]
  (let [cond-func (fn [conn]
                    (let [vessel (-> conn SpaceCenter/newInstance .getActiveVessel)
                          call (.getCall conn (.getOrbit vessel) "getApoapsisAltitude" (into-array Object '()))]
                      (KRPC$Expression/greaterThan conn (KRPC$Expression/call conn call) (KRPC$Expression/constantDouble conn altitude))))]
    (wait-for cond-func)))

(defn gravity-turn
  "Performs a uniform gravity turn between the start and end altitudes, pointed towards the given heading.
   
  See https://en.wikipedia.org/wiki/Gravity_turn for more discussion on why this is important for a climing rocket."
  [turn-start-altitude turn-end-altitude heading]
  (fn [conn]
    (let [sc (SpaceCenter/newInstance conn)
          vessel (.getActiveVessel sc)
          altitude (.addStream conn (.flight vessel (.getReferenceFrame vessel)) "getSurfaceAltitude" (into-array Object '()))
          total-altitude (- turn-end-altitude turn-start-altitude)]
      (loop []
        (Thread/sleep 1000)
        ; The gravity turn is a parameterized curve from t = 0 to t = 1, where t is the progress of the turn.
        ;
        ; The equation is angle(t) = 90 - 90t, such that angle(0) = 90 and angle(0) = 0.
        ; Substituting t = (current-altitude - turn-start-altitude) / (turn-end-altitude - turn-start-altitude) aka (total-altitude)
        ; We get:
        ; angle(current-altitude) = 90 - 90((current-altitude - turn-start-altitude) / total-altitude)
        (let [current-altitude (max (.get altitude) turn-start-altitude)
              progress (- current-altitude turn-start-altitude)
              new-angle (- 90 (* 90 (/ progress total-altitude)))]
          (when (<= current-altitude turn-end-altitude)
            (-> vessel .getAutoPilot (.targetPitchAndHeading new-angle heading))
            (recur)))))))

(defn raise-apoapsis
  "Maneuver to raise the apoapsis to the given target altitude, for use in a launch."
  [target-altitude]
  (maneuver
   ; Assuming we're on a trajectory to leave the atmosphere, it's more efficient to burn in vacuum
   (throttle 0.0)
   (wait-for-atmospheric-density 0.0)
   (throttle 1.0)
   (wait-for-apoapsis target-altitude)
   (throttle 0.0)))

(defn circularize-orbit
  "Circularizes the orbit around the current body."
  []
  (fn [conn]
    (let [sc (SpaceCenter/newInstance conn)
          vessel (.getActiveVessel sc)
          orbit (.getOrbit vessel)
          ctrl (.getControl vessel)
          burn-dv (orbit-circularization-cost orbit)
          burn-time (burn-time vessel burn-dv)
          apoapsis-time (+ (.getUT sc) (.getTimeToApoapsis orbit))
          burn-ut (- apoapsis-time (/ burn-time 2.0))
          node (-> vessel .getControl (.addNode apoapsis-time burn-dv 0.0 0.0))
          time-to-apoapsis-stream (.addStream conn orbit "getTimeToApoapsis" (into-array Object '()))
          remaining-burn (.addStream conn node "getRemainingDeltaV" (into-array Object '()))]
      (doto (.getAutoPilot vessel)
        (.setReferenceFrame (.getReferenceFrame node))
        (.setTargetDirection (Triplet. 0.0 1.0 0.0))
        (.wait_))
      (.warpTo sc burn-ut 100000.0 4.0)
      (burn conn burn-time)
      (.setThrottle ctrl 0.05)
      (loop []
        (Thread/sleep 0.1)
        (when (> (.get remaining-burn) 5.0)
          (recur)))
      (.setThrottle ctrl 0.0))))

(defn countdown []
  (maneuver
   (message "5...")
   (sleep 1)
   (message "4...")
   (sleep 1)
   (message "3...")
   (sleep 1)
   (message "2...")
   (sleep 1)
   (message "1...")
   (sleep 1)
   (message "Liftoff!")))

(defn ascent [staging target-altitude heading]
  (maneuver
   (message "Beginning ascent")
   (engage-autopilot)
   (orient 90.0 heading)
   (throttle 1.0)
   (countdown)
   (parallel-maneuver
    staging
    (maneuver
     (gravity-turn 500.0 50000.0 heading)
     (raise-apoapsis target-altitude)
     (circularize-orbit)))
   (message "Ascent complete")
   (disengage-autopilot)))

;; Vessel-specific maneuvers

(def korb-staging
  "Staging for the Korb 1 and 2 rockets. These rockets are single-stage and don't need anything other than the launc stage."
  (maneuver (stage)))

(def korb-3-staging
  "Staging for the Korb 3 rocket. Korb 3 is a three-stage rocket with solid-rocket boosters in the first stage."
  (maneuver
   (stage)
   (wait-for-fuel-level "SolidFuel" 0)
   (stage)
   (wait-for-fuel-level "LiquidFuel" 0)
   (stage)))

(def korb-4-staging
  "Same as the Korb 3, except it doesn't have any solid rocket boosters."
  (maneuver
   (stage)
   (wait-for-fuel-level "LiquidFuel" 0)
   (stage)
   (wait-for-fuel-level "LiquidFuel" 0)
   (stage)))
