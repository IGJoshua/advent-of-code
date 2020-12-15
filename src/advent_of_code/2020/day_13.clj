(ns advent-of-code.2020.day-13
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [net.cgrand.xforms :as x]))

(defn bus-times
  ([id] (cons 0 (bus-times id 0)))
  ([id last-time]
   (lazy-seq
    (let [current-time (+ id last-time)]
      (cons current-time
            (bus-times id current-time))))))

(defn ids-from-line
  [s]
  (map #(Long/parseLong %) (re-seq #"\d+" s)))

(defn part-1
  []
  (let [input (file-lines "day-13.txt")
        early-time (Long/parseLong (nth input 0))
        bus-ids (ids-from-line (nth input 1))
        early-times (sequence (map (comp first
                                         (partial drop-while (partial > early-time))
                                         bus-times))
                              bus-ids)
        early-times (into {} (map vector bus-ids early-times))
        [bus-id arrival-time] (reduce (fn [[curr-id curr-min :as acc] [id val :as elt]]
                                        (if (< val curr-min)
                                          elt
                                          acc))
                                      (seq early-times))]
    (* bus-id (- arrival-time early-time))))

(defn ids-at-idx
  [s]
  (into {} (comp (map second)
                 (map #(try (Long/parseLong %)
                            (catch NumberFormatException e
                              nil)))
                 (map-indexed #(vector (- %1) %2))
                 (filter (comp some? second))
                 (map vec))
        (re-seq #"([^,]+),?" s)))

(defn bezout
  [a b]
  (loop [[old-r r] [a b]
         [old-s s] [1 0]
         [old-t t] [0 1]]
    (if (zero? r)
      [old-s old-t]
      (let [quotient (quot old-r r)]
        (recur [r (-' old-r (*' quotient r))]
               [s (-' old-s (*' quotient s))]
               [t (-' old-t (*' quotient t))])))))

(defn construct-for-pair
  [[a1 n1] [a2 n2]]
  (let [[m1 m2] (bezout n1 n2)]
    (+' (*' a1 m2 n2)
        (*' a2 m1 n1))))

(defn part-2
  []
  (let [[_ ids] (file-lines "day-13.txt")
        ids (ids-at-idx ids)]
    (apply
     mod
     (reduce (fn [v1 v2]
               (let [a12 (construct-for-pair v1 v2)]
                 [a12 (reduce *' (map second [v1 v2]))]))
             (seq ids)))))
