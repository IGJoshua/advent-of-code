(ns advent-of-code.2020.day-16
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.xforms :as x]
   [clojure.set :as set]))

(def input (slurp (io/resource "day-16.txt")))

(defn parse-field
  [line]
  (let [[_ field-name min1 max1 min2 max2 :as match]
        (re-matches #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" line)
        [min1 max1 min2 max2] (map (fnil #(Long/parseLong %) "0") [min1 max1 min2 max2])]
    (when match
      [(keyword (str/replace field-name " " "-"))
       {:small-range [min1 max1]
        :big-range [min2 max2]}])))

(defn parse-input
  [input]
  (let [[fields my-ticket nearby-tickets] (str/split input #"\n\n")
        fields (str/split-lines fields)
        [_ my-ticket] (str/split-lines my-ticket)
        nearby-tickets (->> nearby-tickets
                            str/split-lines
                            (sequence (comp (drop 1)
                                            (map #(str/split % #","))
                                            (map (fn [ticket] (map #(Long/parseLong %) ticket)))
                                            (map (partial into [])))))]
    {:nearby nearby-tickets
     :ticket (map #(Long/parseLong %) (str/split my-ticket #","))
     :fields (into {} (map parse-field)
                   fields)}))

(defn in-ranges?
  [range input]
  (let [[min1 max1] (:small-range range)
        [min2 max2] (:big-range range)]
    (or (<= min1 input max1)
        (<= min2 input max2))))

(defn part-1
  []
  (let [input (parse-input input)
        all-fields (flatten (:nearby input))
        ranges (vals (:fields input))]
    (transduce (remove (fn [field] (some #(in-ranges? % field) ranges)))
               + 0 all-fields)))

(defn matching-ranges
  [ranges value]
  (filter #(in-ranges? (ranges %) value) (keys ranges)))

(def single-count (comp (partial = 1) count))

(defn determine-fields
  [fields]
  (assert (every? #(> (count %) 0) fields))
  (loop [fields fields
         removed #{}]
    (if (every? single-count fields)
      (map first fields)
      (let [single (first
                    (first
                     (filter (comp
                              #(every? identity %)
                              (juxt (comp (complement removed) first)
                                    single-count))
                             fields)))
            fields (map (fn [field]
                          (if (single-count field)
                            field
                            (disj field single)))
                        fields)]
        (recur fields (conj removed single))))))

(defn part-2
  []
  (let [input (parse-input input)
        ranges (:fields input)
        nearby (:nearby input)
        nearby (filter (partial every? (fn [field] (some #(in-ranges? % field) (vals ranges)))) nearby)
        matching-fields (map (partial map (partial matching-ranges ranges)) nearby)
        fields (map #(map (fn [ticket] (nth ticket %)) matching-fields)
                    (range (count (first matching-fields))))
        fields (map #(reduce set/intersection (map set %)) fields)
        fields (determine-fields fields)
        field-indices (into {} (map-indexed (comp vec reverse vector)) fields)
        departure-fields (filter (comp #(str/starts-with? % "departure") name) fields)
        ticket (:ticket input)]
    (reduce * (map #(nth ticket (field-indices %)) departure-fields))))
