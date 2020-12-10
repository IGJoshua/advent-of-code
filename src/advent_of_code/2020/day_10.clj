(ns advent-of-code.2020.day-10
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [net.cgrand.xforms :as x]))

(defn part-1
  []
  (let [input (file-lines "day-10.txt")
        input (into [0] (sort (map #(Long/parseLong %) input)))
        max-val (nth input (dec (count input)))
        input (conj input (+ max-val 3))
        diff-of-n (fn [n]
                    (count
                     (sequence (comp (x/partition 2 1)
                                     (filter #(= n (Math/abs (apply - %)))))
                               input)))
        diff-of-1 (diff-of-n 1)
        diff-of-3 (diff-of-n 3)]
    (* diff-of-1 diff-of-3)))

(defn removable-index
  [input idx]
  (let [s (when (nat-int? (dec idx))
            (nth input (dec idx)))
        e (when (< (inc idx) (count input))
            (nth input (inc idx)))]
    (when (and s e)
      (<= (- e s) 3))))

(defn num-paths
  [input idx paths]
  (if (= idx (count input))
    1
    (apply +' (take (if (removable-index input idx)
                      (count (take-while #(<= (- % (nth input (dec idx))) 3)
                                         (drop idx input)))
                      1)
                    paths))))

(defn part-2
  []
  (let [input (file-lines "day-10.txt")
        input (into [0] (sort (map #(Long/parseLong %) input)))
        max-val (+ 3 (nth input (dec (count input))))
        input (conj input max-val (inc max-val))]
    (loop [idx (count input)
           paths ()]
      (if (pos? idx)
        (recur (dec idx) (conj paths (num-paths input idx paths)))
        (num-paths input idx paths)))))
