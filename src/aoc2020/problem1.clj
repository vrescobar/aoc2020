(ns aoc2020.problem1
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))


(def text (slurp "resources/problem1.txt"))
;; (def demo "1721\n979\n366\n299\n675\n1456")

(defn parse [text]
  (->> text
       (str/split-lines)
       (map #(Integer/parseInt %))))

(def sums2020? #(= (apply + %) 2020))
(def filter-sums2020 (partial filter sums2020?))
(def product (partial map #(apply * %)))
(defn problem1 [l]
  (-> l
      (parse)
      (combo/combinations 2)
      (filter-sums2020)
      (product)
      (first)))

(def solution (problem1 text))