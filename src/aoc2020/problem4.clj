(ns aoc2020.problem4
  (:require
   [clojure.string :as str]
   [clojure.core.match :refer [match]]))

(def input (slurp "resources/problem4.txt"))
#_(def demo_p1 "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn text [input]
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
                              (map valid-passport? (text input)))))
;;;;;;;;;; part 2
(def p2-validators
  (let [between #(<= %1 %3 %2)]
    ; four digits; at least 1920 and at most 2002.
    {:byr #(->> %
                (re-matches #".*byr:(\d{4}).*")
                (second)
                (Integer/parseInt)
                (between 1920 2002))
     ; four digits; at least 2010 and at most 2020.
     :iyr #(->> %
                (re-matches #".*iyr:(\d{4}).*")
                (second)
                (Integer/parseInt)
                (between 2010 2020))
     ; four digits; at least 2020 and at most 2030.
     :eyr #(->> %
                (re-matches #".*eyr:(\d{4}).*")
                (second)
                (Integer/parseInt)
                (between 2020 2030))
     ; (Height) - a number followed by either cm or in:
     ;       If cm, the number must be at least 150 and at most 193.
     ;      If in, the number must be at least 59 and at most 76.
     :hgt #(match (re-find #".*hgt:(\d+)(in|cm).*" %)
             [_ cm "cm"] (->> cm
                              (Integer/parseInt)
                              (between 150 193))
             [_ in "in"] (->> in
                              (Integer/parseInt)
                              (between 59 76))
             :else false)
     ; a # followed by exactly six characters 0-9 or a-f.
     :hcl #(string? (second (re-matches #".*hcl:#([0-9a-f]{6}).*" %)))
     ; amb blu brn gry grn hzl oth
     :ecl #(string? (second (re-matches #".*ecl:(amb|blu|brn|gry|grn|hzl|oth).*" %)))
     ; a nine-digit number, including leading zeroes.
     :pid #(second (re-matches #".*pid:(\d{9})(\D+.*|$)" %))
     ; ignored, missing or not.
     :cid (fn [_] true)}))


(defn p2-valid-field? [pass field]
  (try ((get p2-validators field) pass) (catch Exception _ (get fields field))))



(def example_invalid_input (text "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"))

(def example_valid_input (text "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))


(defn p2-valid-pass? [pass]
  (every? (partial p2-valid-field? pass) (keys fields)))

;; Test case
(not-any? p2-valid-pass? example_invalid_input)
(every? p2-valid-pass? example_valid_input)

(def solution2
  (count (filter p2-valid-pass? (text input))))

