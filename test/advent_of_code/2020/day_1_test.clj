(ns advent-of-code.2020.day-1-test
  (:require [advent-of-code.2020.day-1 :as sut]
            [clojure.test :as t]))

(t/deftest sum-to-returns-vec
  (t/is (= '([1 2]) (sut/numbers-sum-to 3 2 [1 2])))
  (t/is (= '([1721 299]) (sut/numbers-sum-to 2020 2 [1721 979 366 299 675 1456]))))

(t/deftest stars
  (t/is (= 326211 (sut/part-1)))
  (t/is (= 131347190 (sut/part-2))))
