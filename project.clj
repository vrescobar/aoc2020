(defproject aoc2020 "0.1.0-SNAPSHOT"
  :description "Advent od Code 2020"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 ; TODO: look why it is broken
                 ; https://github.com/greglook/whidbey
                 ;[mvxcvi/whidbey "2.2.1"]
                 ]
  :main aoc2020.main/main
  :repl-options {:port 9000
                 :host "localhost"
                 ;:middleware [whidbey.plugin/repl-pprint]
                 ;:init-ns aoc2020.repl
                 })
                 
                 
