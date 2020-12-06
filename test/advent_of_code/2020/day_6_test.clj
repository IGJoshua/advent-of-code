(ns advent-of-code.2020.day-6-test
  (:require [advent-of-code.2020.day-6 :as sut]
            [clojure.test :as t]))

(t/deftest test-input
  (t/is (= [3 3 3 1 1] (map count (sut/parse-input "abc

a
b
c

ab
ac

a
a
a
a

b")))))

(t/deftest test-input-2
  (t/is (= [3 0 1 1 1] (map count (sut/parse-input-2 "abc

a
b
c

ab
ac

a
a
a
a

b")))))
