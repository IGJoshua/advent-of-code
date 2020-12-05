(ns advent-of-code.2020.day-5
  "Day five of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]))

(defn binary-search-idx
  "Find the index of an item in a given range assuming a binary search taking the path."
  ([path] (binary-search-idx path [0 128]))
  ([path [mn mx]]
   (let [mid (+ mn (/ (- mx mn) 2))]
     (cond
       (empty? path) mn
       (= mn mx) mn
       (= :lt (first path)) (binary-search-idx (rest path) [mn mid])
       (= :gt (first path)) (binary-search-idx (rest path) [mid mx])
       :otherwise nil))))

(defn seat-num
  "Get the seat row and column number for the given input ticket string."
  [s]
  (let [[_ r c] (re-matches #"([BF]{7})([RL]{3})" s)
        r (map {\B :gt \F :lt} r)
        c (map {\R :gt \L :lt} c)]
    [(binary-search-idx r)
     (binary-search-idx c [0 8])]))

(defn seat-id
  "Get the seat id of the given row and column number."
  [[r c]]
  (+ (* r 8) c))

(defn part-1
  "Solve part 1 of the puzzle."
  []
  (let [input (file-lines "day-5.txt")]
    (transduce (comp (map seat-num)
                     (map seat-id))
               max
               Integer/MIN_VALUE
               input)))

(defn part-2
  "Solve part 2 of the puzzle."
  []
  (let [input (file-lines "day-5.txt")
        seat-ids (into #{}
                       (comp (map seat-num)
                             (map seat-id))
                       input)
        min-id (reduce min seat-ids)
        max-id (reduce max seat-ids)]
    (first
     (filter #(and (contains? seat-ids (inc %))
                   (contains? seat-ids (dec %))
                   (not (contains? seat-ids %)))
             (range min-id max-id)))))

;; =================================================================
;; High Performance Version

(def ticket->int-xf
  "Transducer for turning ticket strings into their integer ids."
  (comp (map (partial map {\B \1 \F \0 \R \1 \L \0}))
        (map (partial apply str))
        (map #(Integer/parseInt % 2))))

(defn part-1'
  "Solve part 1 of the puzzle."
  []
  (let [input (file-lines "day-5.txt")]
    (transduce ticket->int-xf max 0 input)))

(defn part-2'
  "Solve part 2 of the puzzle."
  []
  (let [input (file-lines "day-5.txt")
        ids (sort <= (sequence (comp ticket->int-xf) input))]
    (reduce
     (fn [cur next]
       (if (= next (+ cur 2))
         (reduced (inc cur))
         next))
     (first ids)
     (rest ids))))
