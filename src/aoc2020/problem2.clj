(ns aoc2020.problem2)

(def text (slurp "resources/problem2.txt"))
;; (def demo "1-3 a: abcde
;; 1-3 b: cdefg
;; 2-9 c: ccccccccc")

(defn amount-chars [chr passwd] (count (filter #(= chr %) passwd)))
(defn amount-or-nil [[_ min max letter passwd]]
  (let [amount (amount-chars (first letter) passwd)]
    (if (<= (Integer/parseInt min) amount (Integer/parseInt max)) passwd nil)))

(defn problem1 [t]
  (->> t
       (map amount-or-nil)
       (filter (complement nil?))))


;;;;;;;;;;;;
(defn position-matches [pos letter password]
  (try
    (= letter (nth password pos))
    (catch StringIndexOutOfBoundsException _
      false)))

(defn second-criteria [[_ min max letter passwd]]
  (let [fst (position-matches (dec (Integer/parseInt min)) (first letter) passwd)
        snd (position-matches (dec (Integer/parseInt max)) (first letter) passwd)]
    (not= fst snd)))

(defn problem2 [t]
  (->> t
       (filter second-criteria)
       (map #(nth % 4))))

(let [parsed (re-seq #"(\d+)-(\d+)\s(\w):\s(.*)" text)]
  (def solution1 (count (problem1 parsed)))
  (def solution2 (count (problem2 parsed))))