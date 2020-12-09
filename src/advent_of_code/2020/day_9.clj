(ns advent-of-code.2020.day-9
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [net.cgrand.xforms :as x]))

(defn has-parity?
  [v]
  (let [nums (butlast v)
        parity (nth v (dec (count v)))]
    (some?
     (seq
      (for [x (range (count nums))
            y (range (count nums))
            :when (> y x)
            :let [x (nth nums x)
                  y (nth nums y)]
            :when (= (+ x y) parity)]
        true)))))

(defn non-parity-values
  [coll part-size]
  (sequence (comp (x/partition (inc part-size) 1)
                  (map-indexed vector)
                  (filter (comp not has-parity? second))
                  (map (fn [[i v]] [(+ i part-size) (nth v part-size)])))
            coll))

(defn part-1
  []
  (let [input (file-lines "day-9.txt")
        input (map #(Long/parseLong %) input)]
    (second (first (non-parity-values input 25)))))

(defn part-2
  []
  (let [input (file-lines "day-9.txt")
        input (map #(Long/parseLong %) input)
        [bad-idx bad-num] (first (non-parity-values input 25))]
    (first
     (for [x (range bad-idx)
           y (range bad-idx)
           :when (> y (inc x))
           :let [subseq (take (- y x) (drop x input))]
           :when (= bad-num (reduce + 0 subseq))]
       (+ (apply min subseq) (apply max subseq))))))
