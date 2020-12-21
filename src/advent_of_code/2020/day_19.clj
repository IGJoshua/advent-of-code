(ns advent-of-code.2020.day-19
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-rule
  [line]
  (let [[_ id rule] (re-matches #"(\d+): (.*)$" line)
        alts (sequence (comp (map str/trim)
                             (map (partial re-seq #"\S+"))
                             (map (partial map edn/read-string)))
                       (str/split rule #"\|"))]
    [(Long/parseLong id) alts]))

(defn parse-rules
  [lines]
  (into {} (map parse-rule) lines))

(defn message-valid-for-rule?
  [rules rule msg]
  (letfn [(elt-valid? [elt msg]
            (if (string? elt)
              (when (str/starts-with? msg elt)
                [(subs msg (count elt))])
              (sequence (comp (keep (partial alt-valid? msg))
                              cat)
                        (rules elt))))
          (alt-valid? [msg alt]
            (reduce (fn [matches elt]
                      (if-let [res (seq (mapcat (partial elt-valid? elt) matches))]
                        res
                        (reduced nil)))
                    [msg] alt))]
    (first
     (sequence (comp (map (partial alt-valid? msg))
                     (map #(map count %))
                     (keep #(some zero? %)))
               (rules rule)))))

(defn part-1
  []
  (let [[rules input] (str/split (slurp (io/resource "day-19.txt")) #"\n\n")
        rules (parse-rules (str/split-lines rules))
        input (str/split-lines input)]
    (count (filter (partial message-valid-for-rule? rules 0) input))))

(defn part-2
  []
  (let [[rules input] (str/split (slurp (io/resource "day-19.txt")) #"\n\n")
        rules (merge (parse-rules (str/split-lines rules))
                     {8 '((42) (42 8))
                      11 '((42 31) (42 11 31))})
        input (str/split-lines input)]
    (count (filter (partial message-valid-for-rule? rules 0) input))))
