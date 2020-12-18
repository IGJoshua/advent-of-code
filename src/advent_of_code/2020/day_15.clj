(ns advent-of-code.2020.day-15
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.string :as str]))

(defn lazy-nums
  [acc idx prev]
  (let [curr-num (if-let [last-seen (acc prev)]
                   (- idx last-seen)
                   0)]
    (lazy-seq
     (cons curr-num
           (lazy-nums (assoc acc prev idx)
                      (inc idx)
                      curr-num)))))

(defn spoken-nums
  [init]
  (let [starting-acc (into {} (comp (map-indexed vector)
                                    (map reverse)
                                    (map vec))
                           (butlast init))]
    (concat init (lazy-nums starting-acc (dec (count init)) (last init)))))

(def input (->> (str/split (first (file-lines "day-15.txt")) #",")
                (map #(Long/parseLong %))))

(def input-nums (spoken-nums input))

(defn part-1
  []
  (nth input-nums (dec 2020)))

(defn part-2
  []
  (nth input-nums (dec 30000000)))
