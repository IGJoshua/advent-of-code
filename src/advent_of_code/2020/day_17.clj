(ns advent-of-code.2020.day-17
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.math.combinatorics :as c]))

(defn neighbor-positions
  [pos]
  (let [pos-ranges (map #(range (dec %) (+ 2 %)) pos)]
    (disj (persistent!
           (transduce (map vec)
                      conj! (transient #{})
                      (apply c/cartesian-product pos-ranges)))
          pos)))

(defn live-neighbors
  [state pos]
  (persistent!
   (transduce (filter state)
              conj! (transient #{})
              (neighbor-positions pos))))

(defn dead-neighbors
  [state pos]
  (persistent!
   (transduce (remove state)
              conj! (transient #{})
              (neighbor-positions pos))))

(defn should-spawn?
  [state potential]
  (= 3 (count (live-neighbors state potential))))

(defn should-die?
  [state live]
  (not
   (<= 2
       (count (live-neighbors state live))
       3)))

(defn step
  [state]
  (let [potential-live (transduce (map (partial dead-neighbors state))
                                  into #{} state)]
    (into (persistent!
           (transduce (filter (partial should-spawn? state))
                      conj! (transient #{}) potential-live))
          (remove (partial should-die? state))
          state)))

(defn parse-slice
  [s]
  (persistent!
   (transduce (mapcat (fn [row]
                        (keep (fn [idx]
                                (case (nth (nth s row) idx)
                                  \. nil
                                  \# [idx row]))
                              (range (count (first s))))))
              conj! (transient #{})
              (range (count s)))))

(defn part-1
  []
  (let [input (file-lines "day-17.txt")
        input (set (map #(conj % 0) (parse-slice input)))
        states (iterate step input)]
    (count (nth states 6))))

(defn part-2
  []
  (let [input (file-lines "day-17.txt")
        input (set (map #(concat % [0 0]) (parse-slice input)))
        states (iterate step input)]
    (count (nth states 6))))
