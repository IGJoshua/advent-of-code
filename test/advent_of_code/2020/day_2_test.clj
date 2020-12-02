(ns advent-of-code.2020.day-2-test
  (:require [advent-of-code.2020.day-2 :as sut]
            [clojure.test :as t]))

(t/deftest requirements
  (t/is (sut/meets-requirements? {:char \a :fst 1 :snd 3} "abcde"))
  (t/is (not (sut/meets-requirements? {:char \b :fst 1 :snd 3} "cdefg")))
  (t/is (sut/meets-requirements? {:char \c :fst 2 :snd 9} "ccccccccc")))

(t/deftest part-1
  (t/is (= 396 (sut/part-1))))

(t/deftest new-requirements
  (t/is (sut/meets-new-requirements? {:char \a :fst 1 :snd 3} "abcde"))
  (t/is (not (sut/meets-new-requirements? {:char \b :fst 1 :snd 3} "cdefg")))
  (t/is (not (sut/meets-new-requirements? {:char \b :fst 2 :snd 9} "ccccccccc"))))

(t/deftest part-2
  (t/is (= 428 (sut/part-2))))
