(ns advent-of-code.2020.day-2
  "Day two of the Advent of Code"
  (:require
   [advent-of-code.2020.util :refer [file-lines]]))

(defn meets-requirements?
  "Verifies the given `char` appears within `fst` to `snd` times in the `in` string."
  [{:keys [char fst snd]} in]
  (when-let [count ((frequencies in) char)]
    (<= fst count snd)))

(defn parse-input
  "Parses an input string into a requirements map and the password.

  The input string must be of the form \"1-2 a: pswd\".

  Returns a vector of the following shape: [{:char char? :fst int? :snd int?} string?]."
  [s]
  (let [[_ fst snd char in] (re-matches #"(\d+)-(\d+) (\S): (\S+)" s)]
    [{:char (nth char 0)
      :fst (Integer/parseInt fst)
      :snd (Integer/parseInt snd)}
     in]))

(defn valid-passwords
  "Given a predicate and sequence of input strings, return the valid passwords.

  The input strings must be valid to pass to `parse-input`, "
  [pred to-test]
  (sequence (comp (map parse-input)
                  (filter (partial apply pred))
                  (map second))
            to-test))

(defn part-1
  "Get the value for the part-1 input."
  []
  (let [f (file-lines "day-2.txt")]
    (count (valid-passwords meets-requirements? f))))

(defn meets-new-requirements?
  "Verifies the `char` appears at exactly one of `fst` or `snd` in the `in` string."
  [{:keys [char fst snd]} in]
  (->> [fst snd]
       (map #(nth in (dec %)))
       (filter #(= char %))
       count
       (= 1)))

(defn part-2
  "Get the value for the part-2 input."
  []
  (let [f (file-lines "day-2.txt")]
    (count (valid-passwords meets-new-requirements? f))))
