(ns aoc2020.main
  (:require [aoc2020.problem1 :as p1]
            [aoc2020.problem2 :as p2]
            [aoc2020.problem3 :as p3]
            [aoc2020.problem4 :as p4]
            [aoc2020.problem5 :as p5]
            [aoc2020.problem6 :as p6]
            [clojure.string :as string]))

(defn main []
   (print 
     (string/join "\n" (list (str "Problem 01/1: "  p1/solution1)
                             (str "Problem 01/2: "  p1/solution2)
                             (str "Problem 02/1: "  p2/solution1)
                             (str "Problem 02/2: "  p2/solution2)
                             (str "Problem 03/1: "  p3/solution1)
                             (str "Problem 03/2: "  p3/solution2)
                             (str "Problem 04/1: "  p4/solution1)
                             (str "Problem 04/2: "  p4/solution2)
                             (str "Problem 05/1: "  p5/solution1)
                             (str "Problem 05/2: "  p5/solution2)
                             (str "Problem 06/1: "  p6/solution1)
                             (str "Problem 06/2: "  p6/solution2)))))
