(ns aoc2020.problem7
  (:require
   [clojure.string :as str :refer [split-lines]]
   [loom.graph :as loom :refer [weighted-digraph predecessors* weight*]]
   [loom.io :refer [view]]
   [clojure.core.match :refer [match]]
   [clojure.set :as set]
   [clojure.test :refer [testing is]]
   ))

(def text (slurp "resources/problem7.txt"))
(def demo1 "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

;; Grammar defition (small incremental parser.)
(let [;; Incrmental parser definitions
      blank "\\s"
      rest-of-sentence "(.*)"
      bag-name "(\\w+\\s\\w+)\\sbags?"
      bag-relation "contain"
      end-of-sentence "\\.$"
      beginning-of-sentence (str "^" bag-name blank bag-relation blank rest-of-sentence end-of-sentence)
      no-more "no other bags"
      bag-amount "(\\d+)"
      list-bags (str bag-amount blank bag-name ",?" blank "?" "(.*)")
      ;; Helper for the viewer
      bag->kw #(keyword (str/replace %1 #"\s" "_"))]


  (defn bag-relations [sentence]
    ^{:doc "The predicate of the sentence in a recursive fashion"}
    ;; More complex grammars may need a loop-recur construct and recursive function calls
    (if (= no-more sentence) []  ;; special base case
        (match (re-matches (re-pattern list-bags) sentence)
          [_, amount, bag, ""] [(bag->kw bag) (Integer/parseInt amount)]
          [_, amount, bag, more-bags] (concat
                                       [(bag->kw bag) (Integer/parseInt amount)]
                                       (bag-relations more-bags)))))
  (defn grammar [sentence]
    ^{:doc "Gets an inputting string/sentence and
   returns a map representing a directed weighted graph
   with a keyword for each bag name and a vector or bag-name integer maps
   
   light red bags contain 1 bright white bag, 2 muted yellow bags.
   {:light_red [:bright_white 1
                :muted_yellow 2]}"}
    (match (re-matches (re-pattern beginning-of-sentence) sentence)
      [_, bag, children] (hash-map (bag->kw bag) (into [] (bag-relations children)))
      :else {:unmatched sentence})))


(defn ast->loomWDG [ast]
  ^{:doc "Tranform parsed text to loom's format"}
  (loop [k ((comp first keys) ast)
         v ((comp first vals) ast)
         coll '()]
    (cond
      ;; Base case: a bag without children
      (and (= [] v)
           (= coll '())) (list k)
      ;; Recursion base case: no more children
      (= [] v) coll
      :else (let [child-bag (first v)
                  amount (second v)]
              (recur k
                     (drop 2 v)
                     (cons [k child-bag amount] coll))))))
(testing
  (testing "Grammar parsing"
   (is (= (grammar "light red bags contain 1 bright white bag, 2 muted yellow bags.")
          {:light_red [:bright_white 1 :muted_yellow 2]}))
    (is (= (grammar "faded blue bags contain no other bags.")
           {:faded_blue []})))
  (testing "AST transformation test"
    (is (= (ast->loomWDG
            {:light_red [:bright_white 1 :muted_yellow 2]})
           (list [:light_red :muted_yellow 2] [:light_red :bright_white 1])))
    (is (= (ast->loomWDG {:faded_blue []})
           (list :faded_blue)))))

(defn graph-problem1 [text]
  (apply weighted-digraph
         (reduce concat
                 (map (comp ast->loomWDG grammar)
                      (split-lines text)))))

(defn total_predecessors [g edge]
  (let [preds (predecessors* g edge)]
    (reduce set/union
            preds
            (map #(total_predecessors g %) preds))))

(def solution1 (str (count (total_predecessors (graph-problem1 text) :shiny_gold))))

(defn total_predecessors [g edge]
  (let [preds (predecessors* g edge)]
    (reduce set/union
            preds
            (map #(total_predecessors g %) preds))))


;;;;;;;;;;;;;;;;;;;;;;;;
(def demo2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defn childrenWeight 
  ([graph node]
   (let [weight (memoize #(weight* graph %1 %2))
         children (memoize #(loom/successors graph %1))]
     (childrenWeight weight children 1 node)))
  ([weight children factor node]
   (let [succs (children node)]
     (if (empty? succs)
       0 ;; no sub-bags
       (reduce +
               (concat
                ;; A single collection of bags
                (map #(* factor %)
                     (map #(weight node %) succs))
                ;; Multiple nested bags
                (map (fn [sub-node]
                       (childrenWeight weight children
                                       (* factor (weight node sub-node)) sub-node))
                     succs)))))))


(def wgd (graph-problem1 demo1))
;(view wgd) ;; Generate png with GraphViz and open PNG

(def wgd2 (graph-problem1 demo2))
;(view wgd2) 

(testing "children weight test for demo text 1"
  (is (= 0 (childrenWeight wgd :faded_blue)))
  (is (= 0 (childrenWeight wgd :dotted_black)))
  (is (= 11 (childrenWeight wgd :vibrant_plum)))
  (is (= 7 (childrenWeight wgd :dark_olive)))
  (is (= 32 (childrenWeight wgd :shiny_gold)))
  (is (= 126 (childrenWeight wgd2 :shiny_gold))))


(def solution2 (childrenWeight (graph-problem1 text) :shiny_gold))
