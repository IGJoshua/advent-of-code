(ns advent-of-code.2020.day-18
  (:require
   [advent-of-code.2020.util :refer [file-lines]]
   [instaparse.core :as insta]
   [clojure.java.io :as io])
  (:refer-clojure :exclude [eval]))

(def grammar (insta/parser (slurp (io/resource "day-18.grammar"))))

(defmulti eval
  (fn [expr]
    (first expr)))

(defmethod eval :default
  [expr]
  (eval (second expr)))

(defmethod eval :Val
  [expr]
  (Long/parseLong (second expr)))

(def ops
  {"+" +
   "-" -
   "/" /
   "*" *})

(defmethod eval :Op
  [expr]
  (ops (second expr)))

(defmethod eval :Expr
  [expr]
  (let [[_ operand1 op operand2 & expr] expr
        res ((eval op) (eval operand1) (eval operand2))]
    (if (seq expr)
      (eval (apply vector :Expr [:Operand [:Val (str res)]] expr))
      res)))

(defn part-1
  []
  (let [input (file-lines "day-18.txt")]
    (transduce (comp (map (partial insta/parse grammar))
                     (map eval))
               + 0
               input)))

(def grammar-with-precidence (insta/parser (slurp (io/resource "day-18.part-2.grammar"))))

(defmulti eval'
  (fn [expr]
    (first expr)))

(defmethod eval' :default
  [expr]
  (eval' (second expr)))

(defmethod eval' :MulExpr
  [[_ & ops]]
  (apply * (map eval' ops)))

(defmethod eval' :AddExpr
  [[_ & ops]]
  (apply + (map eval' ops)))

(defmethod eval' :Val
  [expr]
  (Long/parseLong (second expr)))

(defn part-2
  []
  (let [input (file-lines "day-18.txt")]
    (transduce (comp (map (partial insta/parse grammar-with-precidence))
                     (map eval'))
               + 0
               input)))
