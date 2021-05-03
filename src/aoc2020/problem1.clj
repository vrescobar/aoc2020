(ns aoc2020.problem1
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))


(def text (slurp "resources/problem1.txt"))
;(def demo "1721\n979\n366\n299\n675\n1456")

(defn parse [text]
  (->> text
       (str/split-lines)
       (map #(Integer/parseInt %))))

(def sums2020? #(= (reduce + %) 2020))
(def filter-sums2020 (partial filter sums2020?))
(def product (partial map (partial reduce *)))

(defn problem [l n]
  (-> l
      (combo/combinations n)
      (filter-sums2020)
      (product)
      (first)))

(let [t (parse text)]
  (def solution1 (problem t 2))
  (def solution2 (problem t 3)))