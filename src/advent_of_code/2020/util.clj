(ns advent-of-code.2020.util
  "Utility functions used accross many days."
  (:require
   [clojure.java.io :as io]))

(defn file-lines
  "Returns a lazy sequence of strings for each line in the given resource."
  [fname]
  (line-seq (io/reader (io/resource fname))))
