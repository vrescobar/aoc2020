(ns aoc2020.problem4
  (:require
   [clojure.string :as str]))

#_(use 'clojure.tools.trace)

(def input (slurp "resources/problem4.txt"))
(def demo "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def text
  (map #(str/join " " %) ; rejoin as single line the passports
       (filter #(not= % (list "")) ; remove separators
               (partition-by #(= 0 (count %)) ; passport separator is an empty line; group these fields
                             (str/split-lines input)))))

(def fields 
  "field name, optionality"
  {:byr false ;"Birth Year"
   :iyr false ; "Issue Year"
   :eyr false ;"Expiration Year"
   :hgt false ;"Height"
   :hcl false ;"Hair Color"
   :ecl false ;"Eye Color"
   :pid false ;"Passport ID"
   :cid true}) ;"Country ID"

(def regexes
  "Fields as a vector of regexes"
  (map (fn [[k _]] [k (re-pattern (str (name k) ":(\\S+)"))])
       fields))

(defn valid-fields [regexes text]
  "return a list of vectors to know which fields are valid.
   All fields must be contained although some are optional"
  (map (fn [[key reg]]
         (let [optional-key (key fields)
               not-nil? (comp not nil?)]
           [key
            (or optional-key
                (not-nil? (re-find reg text)))]))
       regexes))



(defn valid-passport? [text]
  "Returns whether the given passport text valid
   or not"
  (every? true? (map second (valid-fields regexes text))))

(def solution1 (count (filter true?
                              (map valid-passport? text))))