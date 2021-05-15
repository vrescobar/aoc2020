(ns aoc2020.problem10
  (:require
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.test :refer [testing is]]
   [clojure.string :refer [split-lines]]
   [clojure.math.combinatorics :refer [combinations]]))


; I should reach 22 joltage
; Adapter, bellow the min/over max: [-1 -2 -3 3]

; Begins at  0, target is 22.
; Rule: in case of multiple paths: the lowest
; Adapters: "16  10  15  5  1  11  7  19  6  12  4"        0   [Diff 1] [Diff 3]
;                           1                           -1 1      X
;                                                2      -3 4              X
;                        3                              -1 5      X
;                                         4             -1 6      X
;                                  5                    -1 7      X
;                 6                                     -3 10             X
;                               7                       -1 11     X
;                                             8         -1 12     X
;                     9                                 -3 15             X
;            10                                         -1 16     X
;                                     11                -3 19             X
;-----------------------------------------------------------------------------
;                                                       -3 20             X
;                                                                 7       5
;-----------------------------------------------------------------------------
; Differences of 1 and 3 times: 35

(def text (slurp "resources/problem10.txt"))
(def demo0 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def demo1 "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(def parse (comp sort
                 #(cons 0 %)
                 ;#(cons 22 %)
                 (partial map #(Integer/parseInt %))
                 split-lines))

(defn alg-steps[text]
  (let [input (parse text)
        deltas (map (fn [[a b]] (- b a))
                    (partition 2 1 input))]
    ;; We still need the last adaptor
    (cons 3 (take-while #(not= % (- 22 3)) deltas))))

(defn problem1 [text]
  (let [{ones 1, threes 3} (frequencies (alg-steps text))]
    (* ones threes)))

(testing
 (testing "Demo 0"
   (is (= (problem1 demo0) 35)))
 (testing "Demo 1"
  (is (= (problem1 demo1) 220))))


(def solution1 (str (problem1 text)))
;(def solution2 "")