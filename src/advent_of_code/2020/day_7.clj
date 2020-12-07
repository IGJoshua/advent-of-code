(ns advent-of-code.2020.day-7
  "Day seven of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]))

(def rule-start #"(?<bagname>.*?) bags contain ")
(def rule-tail-elt #"(?<count>\d+) (?<bagname>.*?) bags?[.,]")

(defn parse-rule
  "Parses "
  [line]
  (let [bagtype (second (re-find rule-start line))
        children (map rest (re-seq rule-tail-elt line))]
    [bagtype (into {} (map (comp (fn [[k v]] [k (Integer/parseInt v)]) reverse)) children)]))

(defn parse-input
  [lines]
  (into {} (map parse-rule) lines))

(defn bag-contains?
  [bags to-test to-contain]
  (if (contains? (bags to-test) to-contain)
    true
    (some #(bag-contains? bags % to-contain) (keys (bags to-test)))))

(defn part-1
  []
  (let [input (file-lines "day-7.txt")
        bags (parse-input input)]
    (count (set (filter #(bag-contains? bags % "shiny gold") (keys bags))))))

(defn contained-count
  [bags bag]
  (let [bag-map (bags bag)]
    (transduce (map #(* (bag-map %) (inc (contained-count bags %)))) + 0 (keys bag-map))))

(defn part-2
  []
  (let [input (file-lines "day-7.txt")
        bags (parse-input input)]
    (contained-count bags "shiny gold")))
