(ns aoc2020.problem9
  (:require
   [clojure.core.match :refer [match]]
   [clojure.test :refer [testing is]]
   [clojure.math.combinatorics :refer [combinations]]))


(def text (slurp "resources/problem9.txt"))
(def demo1 "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse [text]
  (map #(Integer/parseInt %)
       (map second
            (re-seq #"(\d+)" text))))

(defn problem1 [amount-preamble text]
  (let [adds-up (fn [c [a b]] (= c (+ a b)))
        serie (parse text)]
    (loop [serie serie]
      (let [preamble-subset (take amount-preamble serie)
            following (first (drop amount-preamble serie))
            preamble-combos  (combinations preamble-subset 2)]
        (if (nil? (first (filter #(adds-up following %) preamble-combos)))
          following
          (recur (rest serie)))))))

(= (problem1 5 demo1) 127)
(def solution1 (str (problem1 25 text)))