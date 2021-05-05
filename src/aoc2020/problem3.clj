(ns aoc2020.problem3
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))


(def text (slurp "resources/problem3.txt"))
#_(def demo 
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

(defn problem1 [t]
  (->> t
     (map cycle)
     (map-indexed #(first (drop (* %1 3) %2)))
     (next)
     (filter #(= \# %))
     (count)))

(let [t (str/split-lines text)]
  (def solution1 (problem1 t)))
;(def solution2 "")