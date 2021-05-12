(ns aoc2020.problem8
  (:require
   [clojure.core.match :refer [match]]
   [clojure.test :refer [testing is]]))


(def text (slurp "resources/problem8.txt"))
(def demo1 "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parsed [text]
  (into [] (map (fn [[instr num]] [instr, (Integer/parseInt num)])
                (map rest (re-seq #"(\w+) ([+-]+\d+)" text)))))

(defn executor [[tape_pos register] [instruction num]]
  (match [instruction num]
    ["nop", _] [(inc tape_pos) register]
    ["acc", num] [(inc tape_pos) (+ register num)]
    ["jmp", num] [(+ num tape_pos) register]
    :else (printf (str "Unknown instruction: " [instruction num]
                       " At tape position and register: " [tape_pos register]))))

(defn initial-state[text] {:pos 0
                           :memory 0
                           :tape (parsed text)
                           :history []})

(defn infinite-loop? [pos history]
  (boolean (some #{pos} history)))

(defn out-of-tape? [new-pos tape]
   (not (<= 0 new-pos (dec (count tape)))))

(defn emulator-exec-step [{:keys [pos memory tape history] :as old-state}]
  (let [[new-pos new-memo] (executor [pos memory] (nth tape pos))
        new-state {:pos new-pos :memory new-memo
                   :tape tape :history (cons pos history)}]
    (cond
      (infinite-loop? new-pos history) {:exception :infinite-loop
                                        :state old-state}
      (out-of-tape? new-pos tape) {:exception :out-of-tape
                                   :state new-state}
      :else new-state)))

(defn small-turing-machine [state]
  (loop [st state]
    (let [exec (emulator-exec-step st)]
      (if (:exception exec) exec (recur exec)))))

(testing "Little turing machine"
  (testing "Parsing instructions"
    (is (= (parsed demo1)
           [["nop" 0] ["acc" 1] ["jmp" 4] ["acc" 3] ["jmp" -3] ["acc" -99] ["acc" 1] ["jmp" -4] ["acc" 6]])))
  (testing "execution of instructions"
    (is (= (executor [0 0] ["nop" -45])
           [1 0]))
    (is (= (executor [0 0] ["acc" 3])
           [1 3]))
    (is (= (executor [8 5] ["acc" -2])
           [9 3]))
    (is (= (executor [10 0] ["jmp" -3])
           [7 0]))
    (is (= (executor [10 0] ["jmp" 10])
           [20 0]))
    (is (= (executor [10 0] ["jmp" 0])
           [10 0])))
  (testing "Loop detector"
    (is (= (infinite-loop? 5 [0]) false))
    (is (= (infinite-loop? 2 [0 1 5]) false))
    (is (= (infinite-loop? 5 [0 1 3 4 9 10]) false))
    (is (= (infinite-loop? 0 [0 1 3 4 9 10]) true))
    (is (= (infinite-loop? 4 [0 1 3 4 9 10]) true))
    (is (= (get-in
            (emulator-exec-step (initial-state "nop +1"))
            [:state :pos])
           1))
    (is (= (:exception (emulator-exec-step {:pos 1
                                            :memory 0
                                            :tape [["acc" 1] ["jmp" -1]]
                                            :history [0]}))
           :infinite-loop)))
    (testing "Out of tape detector"
      (is (true? (out-of-tape? -1 [0])))
      (is (true? (out-of-tape? 2 [0 1])))
      (is (false? (out-of-tape? 0 [0 1])))
      (is (false? (out-of-tape? 1 [0 1])))
      (is (false? (out-of-tape? 2 [0 1 2 3]))))

    (testing "Simulated small turing machine"
      (is (= (small-turing-machine (initial-state "acc +1"))
             {:exception :out-of-tape
              :state {:pos 1
                      :memory 1
                      :tape [["acc" 1]]
                      :history [0]}}))
      (is (= (:exception (small-turing-machine (initial-state "nop +1\nnop +10\nacc +5\njmp +2\nnop -2\nacc -1\njmp -3")))
             :infinite-loop))
      (is
       (= (small-turing-machine
           (initial-state "acc +1\njmp -1"))
          {:exception :infinite-loop
           :state {:pos 1
                   :memory 1
                   :tape [["acc" 1] ["jmp" -1]]
                   :history [0]}}))
      (is
       (= 5 (get-in (small-turing-machine (initial-state demo1))
                    [:state :memory])))
      (is
       (= (:exception (small-turing-machine (initial-state demo1)))
          :infinite-loop))))


(def solution1 (str (get-in (small-turing-machine (initial-state text))
                            [:state :memory])))
;(def solution2 "")