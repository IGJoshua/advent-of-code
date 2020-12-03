(ns advent-of-code.2020.day-3-test
  (:require [advent-of-code.2020.day-3 :as sut]
            [clojure.test :as t]))

(t/deftest final-results
  (t/is (= 209 (sut/part-1)))
  (t/is (= 1574890240 (sut/part-2))))
