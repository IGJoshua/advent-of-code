(ns advent-of-code.2020.day-4
  "Day four of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [clojure.string :as str]))

(s/def ::byr #(<= 1920 % 2002))
(s/def ::iyr #(<= 2010 % 2020))
(s/def ::eyr #(<= 2020 % 2030))
(s/def ::hgt.num pos-int?)
(s/def ::hgt.unit #{::cm ::in})
(s/def ::hgt (s/and (s/keys :req [::hgt.num ::hgt.unit])
                    #(case (::hgt.unit %)
                       ::cm (<= 150 (::hgt.num %) 193)
                       ::in (<= 59 (::hgt.num %) 76)
                       false)))
(s/def ::hcl (partial re-matches #"#[0-9a-f]{6}"))
(s/def ::ecl #{::amb ::blu ::brn ::gry ::grn ::hzl ::oth})
(s/def ::pid (partial re-matches #"\d{9}"))
(s/def ::cid string?)
(s/def ::passport (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt [::cid]))

(def required-fields #{::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid})

(def ^:dynamic *parsers*
  "A map of parsers for values in passports."
  {})

(defn parse-passport
  "Turns a passport string into a map.

  String must be whitespace separated pairs of the form \"key:value\".

  Values are processed by 1-arity functions in [[*parsers*]] under the keyword
  for the key with the `advent-of-code.2020.day-4` namespace."
  [s]
  (transduce (comp (map #(str/split % #":"))
                   (map (fn [[k v]] [(keyword "advent-of-code.2020.day-4" k) v]))
                   (map (fn [[k v]] [k ((or (*parsers* k) identity) v)])))
             conj
             {}
             (str/split s #"\s+")))

(defn part-1
  "Solves part 1 of the puzzle."
  []
  (let [in (str/join "\n" (file-lines "day-4.txt"))]
    (count
     (sequence
      (comp (map parse-passport)
            (map (comp set keys))
            (map (partial set/difference required-fields))
            (map count)
            (filter zero?))
      (str/split in #"\n\n")))))

(defn part-2
  "Solves part 2 of the puzzle."
  []
  (let [in (str/join "\n" (file-lines "day-4.txt"))
        parse-num #(Integer/parseInt %)]
    (binding [*parsers* {::byr parse-num
                         ::iyr parse-num
                         ::eyr parse-num
                         ::hgt (fn [s]
                                 (when-let [[_ num unit] (re-matches #"(\d+)(cm|in)" s)]
                                   {::hgt.unit (keyword "advent-of-code.2020.day-4" unit)
                                    ::hgt.num (parse-num num)}))
                         ::ecl (partial keyword "advent-of-code.2020.day-4")}]
      (count
       (sequence
        (comp (map parse-passport)
              (map (partial s/valid? ::passport))
              (filter true?))
        (str/split in #"\n\n"))))))
