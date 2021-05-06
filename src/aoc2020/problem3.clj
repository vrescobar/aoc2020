(ns aoc2020.problem3
  (:require
   [clojure.string :as str]))

#_(use 'clojure.tools.trace)

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

(def text (slurp "resources/problem3.txt"))
(def vt (map cycle (str/split-lines text)))

(defn generalized-problem [hdrops vdrops vt]
  (let [max_length (count vt)
        alg-steps (partition 2
                             (interleave
                              (map #(* hdrops %1) (iterate inc 0))
                              (map #(* vdrops %1) (range (inc (int (/ max_length vdrops)))))))
        matches (map (fn [[h v]]
                       (nth (nth vt v) h))
                     alg-steps)]
    (count
     (filter #(= \# %) (take (count vt) matches)))))

(defn problem1 [vt]
  (generalized-problem 3 1 vt))

(defn problem2 [vt]
  (let [args [(generalized-problem 1 1 vt)
              (generalized-problem 3 1 vt)
              (generalized-problem 5 1 vt)
              (generalized-problem 7 1 vt)
              (generalized-problem 1 2 vt)]]
    (reduce * args)))

(def solution1 (problem1 vt))
(def solution2 (problem2 vt))

+