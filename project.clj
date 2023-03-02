(defproject korb "0.1.0"
  :description "Kerbal Space Program orbital computer"
  :url "https://github.com/swgillespie/korb"
  :license {:name "MIT"
            :url "https://opensource.org/license/mit/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.673"]]
  :resource-paths ["third-party/javatuples-1.2.jar", "third-party/krpc-java-0.4.8.jar", "third-party/protobuf-java-4.0.0-rc-2.jar"]
  :repl-options {:init-ns korb.core})
