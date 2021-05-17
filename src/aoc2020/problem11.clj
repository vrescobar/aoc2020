(ns aoc2020.problem11
  (:require
   ;[clojure.pprint :refer [pprint]]
   [clojure.core.match :refer [match]]
   [clojure.test :refer [testing is]]
   [clojure.string :as str]))


(def pos-8-neighbours 
  (memoize
   (fn [last-pos row-size k]
     (let [neg #(* -1 %)
           cur-row (int (/ k row-size))
           keep-between (fn [[min max] col] (filter #(and (<= min %) (< % max)) col))
           default-row-offsets [[(+ -1 (neg row-size)) (neg row-size) (+ 1 (neg row-size))]
                                [-1 1]
                                [(- row-size 1) row-size (+ 1 row-size)]]

           default-row-margins [[(* row-size (dec cur-row)) (* row-size cur-row)]
                                [(* row-size cur-row) (* row-size (inc cur-row))]
                                [(* row-size (inc cur-row)) (* row-size (+ 2 cur-row))]]]
       (->>
        default-row-offsets
        (mapv (fn [v] (mapv #(+ k %) v)))
        (interleave default-row-margins)
        (partition 2)
        (mapv #(apply keep-between %))
        (reduce concat)
        (keep-between [0 last-pos]))))))


(testing "Get neighbours"
  (is (= (pos-8-neighbours 100 10 0) [1 10 11]))
  (is (= (pos-8-neighbours 100 10 1) [0 2 10 11 12]))
  (is (= (pos-8-neighbours 100 10 62) [51 52 53 61 63 71 72 73]))
  (is (= (pos-8-neighbours 100 10 39) [28 29 38 48 49]))
  (is (= (pos-8-neighbours 100 10 70) [60 61 71 80 81]))
  (is (= (pos-8-neighbours 100 10 99) [88 89 98]))
  (is (= (pos-8-neighbours 25 5 4) [3 8 9]))
  (is (= (pos-8-neighbours 25 5 20) [15 16 21])))

(defn transformation [[elem more]]
  (let [amount-occupied (count (filter #(= \# %) more))]
    (cond
      (and (= elem \L) (zero? amount-occupied)) \#
      (and (= elem \# )(<= 4 amount-occupied)) \L
      :else elem)))


(defn row-length[text]
  (str/index-of text "\n"))

(defn parse [text]
  (vec
   (str/join
    (str/split-lines text))))
  

(defn neighbours [offs xs]
  (let [total (count xs)]
    (->> (range 0 total)
         (mapv #(pos-8-neighbours total offs %))
         (mapv (fn [col] (mapv #(nth xs %) col)))
         (interleave (range 0 total))
         (partition 2)
         (mapv (fn [[pos col]] [(nth xs pos) col])))))

(defn one-step [row-lenght text]
  (->> text
       (neighbours row-lenght)
       (mapv transformation)))


(defn amount-used-sits [text]
  (count (filter #(= \# %) text)))

(defn stabilized-game-of-sits [rl text]
  (loop [prev-text text
         text (one-step rl text)]
    (do
      (println (str "Occupied sits: " (amount-used-sits prev-text)))
      (if (= text prev-text) prev-text
          (recur text (one-step rl text))))))

(def demo-steps ["L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL"
                 "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##"
                 "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##"
                 "#.##.L#.##\n#L###LL.L#\nL.#.#..#..\n#L##.##.L#\n#.##.LL.LL\n#.###L#.##\n..#.#.....\n#L######L#\n#.LL###L.L\n#.#L###.##"
                 "#.#L.L#.##\n#LLL#LL.L#\nL.L.L..#..\n#LLL.##.L#\n#.LL.LL.LL\n#.LL#L#.##\n..L.L.....\n#L#LLLL#L#\n#.LLLLLL.L\n#.#L#L#.##"
                 "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##"])

(testing "Example Steps"
  (let [ds (mapv parse demo-steps)
        first-demo (first ds)
        rl (row-length (first demo-steps))]

    (is (= (one-step rl first-demo)
           (second ds)))
    (->> ds
         (partition 2 1)
          (mapv (fn [[f s]]
                 (is (= (one-step rl f)
                        s)))))
    (is (= (last ds)
            (stabilized-game-of-sits rl first-demo)))
    (is (= 37 (amount-used-sits
                (stabilized-game-of-sits rl first-demo))))))
    

(def solution1 
    (let [text (slurp "resources/problem11.txt")
          rl (row-length text)]
      (->> text
           (parse)
           (stabilized-game-of-sits rl)
           amount-used-sits
           str)))

(def solution2 "")
