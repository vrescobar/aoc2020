(ns aoc2020.problem3
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))


(def text (slurp "resources/problem3.txt"))
(def demo
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn horizontal-drops [drops]
  (fn [row col]
  (first (drop (* row drops) col))))
(def repeating-schema 
  (cycle (partition 2 (interleave [1 3 5 7 1] [1 1 1 1 2]))))


(defn generalized-problem [h d t]
 (let [drop-pattern (horizontal-drops h)
       amount-drops d]
   (->> t
        (map cycle)
        (map-indexed drop-pattern)
        (drop amount-drops)
        (filter #(= \# %))
        (count))))

(defn problem1 [t]
  (generalized-problem 3 1 t))

(let [t (str/split-lines demo)]
  (def solution1 (problem1 t)) ;; problem 1 generalized
  (def solution2 "Nope"))

