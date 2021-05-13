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

(defn problem1 [amount-preamble serie]
  (let [adds-up (fn [c [a b]] (= c (+ a b)))]
    (loop [serie serie]
      (let [preamble-subset (take amount-preamble serie)
            following (first (drop amount-preamble serie))
            preamble-combos  (combinations preamble-subset 2)]
        (if (nil? (first (filter #(adds-up following %) preamble-combos)))
          following
          (recur (rest serie)))))))

(def solution1 (str (problem1 25 (parse text))))


(defn eval-candidate [num cont-serie]
  (let [sum (reduce + cont-serie)]
    (cond
      (= num sum) :found
      (< num sum) :next-serie
      (> num sum) :add-more)))

(testing "Eval candidate"
  (is (= :found (eval-candidate 3 [1 2])))
  (is (= :next-serie (eval-candidate 4 [1 2 8])))
  (is (= :add-more (eval-candidate 4 [1 2]))))

(defn find-range [target-num serie]
  (loop [to-take 2
         to-drop 0]
    (let [set-candidate (take to-take (drop to-drop serie))]
      (match (eval-candidate target-num set-candidate)
        :found set-candidate
        :add-more (recur (inc to-take) to-drop)
        :next-serie (recur 2 (inc to-drop))))))


(defn problem2 [window text]
  (->> text
       (parse)
       (#(find-range (problem1 window %1) %1))
       (sort)
       (#(+ (first %1) (last %1)))))

(testing "demo2"
  (is (= (problem2 5 demo1) 62))
  (is (= (problem1 5 (parse demo1)) 127)))

(def solution2 (problem2 25 text))
