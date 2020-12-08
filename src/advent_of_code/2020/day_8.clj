(ns advent-of-code.2020.day-8
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [clojure.string :as str]))

(def instructions
  {"acc" (fn [acc ep arg]
           [(+ arg acc) (inc ep)])
   "jmp" (fn [acc ep arg]
           [acc (+ ep arg)])
   "nop" (fn [acc ep arg]
           [acc (inc ep)])})

(defn parse-instruction
  [line]
  (let [[instr arg] (str/split line #"\s+")]
    [instr (Integer/parseInt arg)]))

(defn part-1
  []
  (let [input (file-lines "day-8.txt")
        program (mapv parse-instruction input)]
    (loop [acc 0
           ep 0
           already-run #{}]
      (if (contains? already-run ep)
        acc
        (when (< ep (count program))
          (let [[instr arg] (nth program ep)
                [acc new-ep] ((instructions instr) acc ep arg)]
            (recur acc new-ep (conj already-run ep))))))))

(defn part-2
  []
  (let [input (file-lines "day-8.txt")
        program (mapv parse-instruction input)
        make-xf (fn [arg]
                  (comp (map-indexed vector)
                        (filter #(= arg (first (second %))))
                        (map first)))
        nop-idxs (sequence (make-xf "nop") program)
        jmp-idxs (sequence (make-xf "jmp") program)
        programs (conj (concat (map #(assoc program % ["jmp" (second (program %))])
                                    nop-idxs)
                               (map #(assoc program % ["nop" (second (program %))])
                                    jmp-idxs))
                       program)]
    (first
     (filter some?
             (for [program programs]
               (loop [acc 0
                      ep 0
                      already-run #{}]
                 (if (contains? already-run ep)
                   nil
                   (if (< ep (count program))
                     (let [[instr arg] (nth program ep)
                           [acc new-ep] ((instructions instr) acc ep arg)]
                       (recur acc new-ep (conj already-run ep)))
                     acc))))))))
