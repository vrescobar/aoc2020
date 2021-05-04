(ns aoc2020.problem2)

(def text (slurp "resources/problem2.txt"))
;; (def demo "1-3 a: abcde
;; 1-3 b: cdefg
;; 2-9 c: ccccccccc")

(defn amount-chars [chr passwd] (count (filter #(= chr %) passwd)))

(defn amount-or-nil [[_ min max letter passwd]]
  (let [amount (amount-chars (first letter) passwd)]
    (if (<= (Integer/parseInt min) amount (Integer/parseInt max)) passwd nil)))

(defn problem2 [t]
  (->> t
       (re-seq #"(\d+)-(\d+)\s(\w):\s(.*)")
       (map amount-or-nil)
       (filter (complement nil?))))

(def solution1 (count (problem2 text)))