(ns advent-of-code.2020.day-6
  "Day six of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.string :as str]
   [clojure.set :as set]))

(def questions (set (map char (range 0x61 0x7a))))

(defn parse-input
  "Counts the number of unique alphabet characters in each paragraph."
  [s]
  (let [groups (str/split s #"\n\n")]
    (map #(set/intersection (set (seq %)) questions) groups)))

(defn part-1
  "Solve part 1 of the puzzle."
  []
  (let [input (str/join "\n" (file-lines "day-6.txt"))
        answers (parse-input input)]
    (transduce (map count) + 0 answers)))

(defn parse-input-2
  "Counts the number of alphabet characters which appear on all lines of each paragraph."
  [s]
  (let [groups (str/split s #"\n\n")
        users (map #(str/split % #"\n") groups)]
    (map #(apply set/intersection (map (comp set seq) %)) users)))

(defn part-2
  "Solve part 2 of the puzzle."
  []
  (let [input (str/join "\n" (file-lines "day-6.txt"))
        answers (parse-input-2 input)]
    (transduce (map count) + 0 answers)))
