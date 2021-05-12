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
                           :history []
                           :prev_state []})

(defn infinite-loop? [pos history]
  (boolean (some #{pos} history)))

(defn out-of-tape? [new-pos tape]
   (not (<= 0 new-pos (dec (count tape)))))

(defn boot-complete? [new-pos tape]
  (= new-pos (count tape)))

(defn emulator-exec-step [{:keys [pos memory tape history prev_state] :as old-state}]
  (let [[new-pos new-memo] (executor [pos memory] (nth tape pos))
        new-state {:pos new-pos :memory new-memo
                   :tape tape :history (cons pos history)
                   :prev_state (cons (dissoc old-state :prev_state) prev_state)}]
    (cond
      (infinite-loop? new-pos history) {:exception :infinite-loop
                                        :state old-state}
      (boot-complete? new-pos tape) {:exception :boot-complete
                                      :state new-state}
      (out-of-tape? new-pos tape) {:exception :out-of-tape
                                   :state old-state}
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
           :infinite-loop))
    (testing "Out of tape detector"
      (is (true? (out-of-tape? -1 [0])))
      (is (true? (out-of-tape? 2 [0 1])))
      (is (false? (out-of-tape? 0 [0 1])))
      (is (false? (out-of-tape? 1 [0 1])))
      (is (false? (out-of-tape? 2 [0 1 2 3]))))

    (testing "Simulated small turing machine"
      (is (= (get-in (small-turing-machine (initial-state "acc +1")) [:state :memory]) 1))
      (is (= 0 (get-in (small-turing-machine (initial-state "jmp +1")) [:state :memory])))
      (is (= (small-turing-machine (initial-state "jmp +2"))
             {:exception :out-of-tape
              :state {:pos 0
                      :memory 0
                      :tape [["jmp" 2]]
                      :prev_states []
                      :history []}}))
      (is (= (:exception (small-turing-machine (initial-state "nop +1\nnop +10\nacc +5\njmp +2\nnop -2\nacc -1\njmp -3")))
             :infinite-loop))
      (is
       (= (small-turing-machine
           (initial-state "acc +1\njmp -1"))
          {:exception :infinite-loop
           :state {:pos 1
                   :memory 1
                   :tape [["acc" 1] ["jmp" -1]]
                   :history [0]
                   :prev_states (list {:pos 0, :memory 0, :tape [["acc" 1] ["jmp" -1]], :history []})}}))
      (is
       (= 5 (get-in (small-turing-machine (initial-state demo1))
                    [:state :memory])))
      (is
       (= (:exception (small-turing-machine (initial-state demo1)))
          :infinite-loop)))))


(def solution1 (str (get-in (small-turing-machine (initial-state text))
                            [:state :memory])))

(def solution-demo1 "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6")

(defn reverse-jmp-nop [op]
  (cond
    (= "jmp" op) "nop"
    (= "nop" op) "jmp"
    :else (println (str "unsupported opperation: " op))))

(defn all-patched-variations [tape]
  (lazy-seq
   (filter (complement nil?)
           (for [[i [op, val]] (zipmap (range) tape)]
             (when (not= op "acc")
               (assoc tape i [(reverse-jmp-nop op) val]))))))

           
(defn multipatch-and-run [original-tape]
  (lazy-seq
   (filter (complement nil?)
          (for [patched-tape (all-patched-variations original-tape)
                :let [state {:pos 0 :memory 0 :tape patched-tape :history [] :prev_states []}
                      execution (small-turing-machine state)]
                ; while is not working as expected, I will do instead a lazy seq
                ;:while (= (:exception execution) :boot-complete)
                ]
            (when (= (:exception execution) :boot-complete)
              execution)))))

(testing "patching demo tape and running it"
  (is (= (get-in (first (multipatch-and-run (parsed demo1)))
                 [:state :memory])
         (get-in (small-turing-machine (initial-state solution-demo1))
                 [:state :memory]))))

(def solution2 (str (get-in (first (multipatch-and-run (parsed text)))
                            [:state :memory])))
