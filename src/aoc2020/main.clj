(ns aoc2020.main
  (:require [aoc2020.problem1 :as p1]
            [clojure.string :as string]))

(defn main []
   (print 
     (string/join "\n" (list (str "Problem 01/1: "  p1/solution1)
                             (str "Problem 01/2: "  p1/solution2)))))
