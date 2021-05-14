(defproject aoc2020 "0.1.0-SNAPSHOT"
  :description "Advent od Code 2020"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/tools.trace "0.7.11"]
                 [org.clojure/core.match "1.0.0"]
                 [aysylu/loom "1.0.2"]
                 ; TODO: look why it is broken
                 ; https://github.com/greglook/whidbey
                 ;[mvxcvi/whidbey "2.2.1"]
                 ]
  :main aoc2020.main
  :aot [aoc2020.main]
  :repl-options {:port 9000
                 :host "localhost"
                 ;:middleware [whidbey.plugin/repl-pprint]
                 ;:init-ns aoc2020.repl
                 }
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[lein-shell "0.5.0"]]}}

  :aliases
  {"native"
   ["shell"
    "native-image" "--report-unsupported-elements-at-runtime"
    "--initialize-at-build-time" "--no-server"
    "-jar" "./target/${:uberjar-name:-${:name}-${:version}-standalone.jar}"
    "-H:Name=./target/${:name}"]})