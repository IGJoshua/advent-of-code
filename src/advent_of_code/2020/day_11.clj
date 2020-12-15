(ns advent-of-code.2020.day-11
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.string :as str]
   [net.cgrand.xforms :as x]))

(def seat-char \L)
(def occupied-char \#)
(def empty-char \.)

(defn parse-state
  [input]
  (into
   {}
   (comp (map-indexed
          (fn [y line]
            (map-indexed (fn [x ch] [[x y] ch]) line)))
         cat)
   input))

(defn neighbors
  [state pos]
  (filter
   some?
   (let [[pos-x pos-y] pos]
     (for [x (map dec (range 3))
           y (map dec (range 3))
           :when (not (every? zero? [x y]))]
       (state [(+ x pos-x) (+ y pos-y)])))))

(defn occupied-neighbors
  [state neighbors]
  (count (filter #{occupied-char} neighbors)))

(defn seat-neighbors
  [state neighbors]
  (count (filter #{seat-char} neighbors)))

(defn step-sim
  [state]
  (into
   {}
   (for [[pos s] (seq state)
         :let [n (neighbors state pos)]]
     [pos
      (cond
        (and (zero? (occupied-neighbors state n))
             (= s seat-char)) occupied-char
        (and (>= (occupied-neighbors state n) 4)
             (= s occupied-char)) seat-char
        :otherwise s)])))

(defn print-state
  [state]
  (let [xs (distinct (sort (map first (keys state))))
        ys (distinct (sort (map first (keys state))))
        max-y (nth ys (dec (count ys)))]
    (print
     (str/join
      (for [x xs
            y ys
            :let [s (state [x y])]]
        (if (= y max-y)
          (str s "\n")
          (str s)))))))

(defn stable-state
  [states]
  (first
   (sequence (comp (x/partition 2 1)
                   (take-while (partial apply not=))
                   (map second)
                   x/last)
             states)))

(defn occupied-chairs
  [state]
  (count (filter #{occupied-char} (vals state))))

(defn part-1
  []
  (let [input (file-lines "day-11.txt")
        state (parse-state input)
        stable-state (stable-state (iterate step-sim state))]
    (occupied-chairs stable-state)))

(defn vec-add
  [[x1 y1] [x2 y2]]
  [(+ x1 x2)
   (+ y1 y2)])

(defn raycast
  [pos dir]
  (lazy-seq (cons pos (raycast (vec-add pos dir) dir))))

(defn raycast-neighbors
  [state pos]
  (filter
   some?
   (for [x (map dec (range 3))
         y (map dec (range 3))
         :when (not (every? zero? [x y]))]
     (first (filter (complement #{empty-char})
                    (map state (rest (raycast pos [x y]))))))))

(defn step-with-raycast
  [state]
  (into
   {}
   (for [[pos s] (seq state)
         :let [n (raycast-neighbors state pos)]]
     [pos
      (cond
        (and (zero? (occupied-neighbors state n))
             (= s seat-char)) occupied-char
        (and (>= (occupied-neighbors state n) 5)
             (= s occupied-char)) seat-char
        :otherwise s)])))

(defn part-2
  []
  (let [input (file-lines "day-11.txt")
        state (parse-state input)
        stable-state (stable-state (iterate step-with-raycast state))]
    (occupied-chairs stable-state)))
