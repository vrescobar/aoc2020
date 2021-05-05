(ns aoc2020.main
  (:require [aoc2020.problem1 :as p1]
            [aoc2020.problem2 :as p2]
            [aoc2020.problem3 :as p3]
            [clojure.string :as string]))

(defn main []
   (print 
     (string/join "\n" (list (str "Problem 01/1: "  p1/solution1)
                             (str "Problem 01/2: "  p1/solution2)
                             (str "Problem 02/1: "  p2/solution1)
                             (str "Problem 02/2: "  p2/solution2)
                             (str "Problem 03/1: "  p3/solution1)
                             ))))
