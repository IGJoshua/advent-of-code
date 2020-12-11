(ns advent-of-code.2020.day-1
  "Day one of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as c]
   [clojure.string :as str]))

(defn numbers-sum-to
  "Returns a list of vectors of numbers which sum to a given value."
  [val choose nums]
  (let [pairs (c/combinations nums choose)]
    (filter #(= val (reduce + %)) pairs)))

(defn part-1
  "Solves the first part of the challenge"
  []
  (let [res (map #(Integer/parseInt %)
                 (file-lines "day-1.txt"))
        vals (first (numbers-sum-to 2020 2 res))]
    (apply * vals)))

(defn part-2
  "Solves the second part of the challenge"
  []
  (let [res (map (comp #(Integer/parseInt %) str/trim)
                 (file-lines "day-1.txt"))
        vals (first (numbers-sum-to 2020 3 res))]
    (apply * vals)))
