(ns advent-of-code.2020.day-3
  "Day three of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]))

(def tree? #{\#})

(defn trees-encountered
  "Counts the number of `tree?`s which are on a path along `m` offset at each step by `d`.

  `d` is a point-vector, of the form [x y].
  `m` is a sequence of sequences representing rows of characters."
  [d m]
  (let [hit-rows (->> m
                      (map-indexed vector)
                      (filter #(zero? (rem (first %) (second d))))
                      (map second))
        reducer (fn [{:keys [pos-x trees]} row]
                  {:pos-x (+ pos-x (first d))
                   :trees (if (tree? (nth row pos-x))
                            (inc trees)
                            trees)})]
    (:trees (reduce reducer {:pos-x 0 :trees 0} hit-rows))))

(defn part-1
  "Solves the first part of the challenge"
  []
  (let [lines (map cycle (file-lines "day-3.txt"))]
    (trees-encountered [3 1] lines)))

(defn part-2
  "Solves the second part of the challenge"
  []
  (let [lines (map cycle (file-lines "day-3.txt"))]
    (apply * (for [slope [[1 1] [3 1] [5 1] [7 1] [1 2]]]
               (trees-encountered slope lines)))))
