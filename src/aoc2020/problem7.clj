(ns aoc2020.problem7
  (:require
   [clojure.string :as str :refer [split-lines]]
   [loom.graph :as loom :refer [weighted-digraph predecessors*]]
   [loom.io :refer [view]]
   [clojure.core.match :refer [match]]
   [clojure.set :as set]))
   
(def text (slurp "resources/problem7.txt"))
#_(def demo1 "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def f (split-lines text))

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
           (= coll '())) (list [k])
      ;; Recursion base case: no more children
      (= [] v) coll
      :else (let [child-bag (first v)
                  amount (second v)]
              (recur k
                     (drop 2 v)
                     (cons [k child-bag amount] coll))))))
;; test cases
(comment
  (every? true? [(= (grammar "light red bags contain 1 bright white bag, 2 muted yellow bags.")
                    {:light_red [:bright_white 1 :muted_yellow 2]})
                 (= (grammar "faded blue bags contain no other bags.")
                    {:faded_blue []})
                 ;; Ast tests
                 (= (ast->loomWDG
                     {:light_red [:bright_white 1 :muted_yellow 2]})
                    (list [:light_red :muted_yellow 2] [:light_red :bright_white 1]))
                 (= (ast->loomWDG {:faded_blue []})
                    (list [:faded_blue]))]))

(defn graph-problem1 [text]
  (apply weighted-digraph
         (reduce concat
                 (map (comp ast->loomWDG grammar)
                      (split-lines text)))))

;(view wgd) ;; Generate png with GraphViz and open PNG
(defn total_predecessors [g edge]
  (let [preds (predecessors* g edge)]
    (reduce set/union
            preds
            (map #(total_predecessors g %) preds))))

(def solution1 (str (count (total_predecessors (graph-problem1 text) :shiny_gold))))
;(def solution2 "")