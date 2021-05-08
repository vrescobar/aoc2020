(ns aoc2020.problem5
  (:require
   [clojure.string :as str]
   [clojure.core.match :refer [match]]))

(def input (str/split-lines (slurp "resources/problem5.txt")))

(defn sit-conversion [sit]
  (match (re-matches #"([F|B]{7})([L|R]{3})" sit)
    [_ row col] {:row (read-string (str "2r" (str/replace row #"F|B" {"F" "0" "B" "1"})))
                 :column (read-string (str "2r" (str/replace col #"L|R" {"L" "0" "R" "1"})))}
    :else nil))

(defn sit-to-id [{r :row c :column}]
  (+ c (* 8 r)))

(def sit-id (comp sit-to-id sit-conversion))


(comment ;demo_p1 test case
 (let [tester (fn [s] [(sit-conversion s), (sit-id s)])]
   (every? true?
           [(= [{:row 44, :column 5} 357] (tester "FBFBBFFRLR"))
            (= [{:row 70, :column 7} 567] (tester "BFFFBBFRRR"))
            (= [{:row 14, :column 7} 119] (tester "FFFBBBFRRR"))
            (= [{:row 102, :column 4} 820] (tester "BBFFBBFRLL"))])))

(def solution1 (str (first (sort > (map sit-id input)))))
(def solution2
  (let [col (sort < (map sit-id input))
        pairs (partition 2 (interleave col (rest col)))
        finder (fn [[prev cur]] (not= (inc prev) cur))]
    ;; filter still has cost N rather than log()
    (inc (first (first (filter finder pairs))))))