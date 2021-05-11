(ns aoc2020.problem6
  (:require
   [clojure.string :as str]
   ; [clojure.core.match :refer [match]]
   [clojure.set :as set]))

(def text (slurp "resources/problem6.txt"))

(defn parse-input [text]
  (filter #(not= % (list ""))
               (partition-by #(= 0 (count %))
                             (str/split-lines text))))

(defn as-sets [grp-answers]
  (into #{} (apply concat
                   (map char-array grp-answers))))

(map as-sets (parse-input text))

(defn problem1 [text]
  (reduce + (map count (map as-sets (parse-input text)))))

(def solution1 (str (problem1 text)))

;;;;;; problem 2 ;;;;;;
#_(def demo2 "abc

a
b
c

ab
ac

a
a
a
a

b")

(defn problem2 [text]
 (reduce +
         (map count (map #(reduce set/intersection %)
                         (map (partial map #(into #{} %)) (parse-input text))))))

(def solution2 (str (problem2 text)))
