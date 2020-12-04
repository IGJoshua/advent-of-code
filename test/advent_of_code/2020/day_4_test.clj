(ns advent-of-code.2020.day-4-test
  (:require [advent-of-code.2020.day-4 :as sut]
            [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.set :as set]))

(def test-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(t/deftest test-input-validates
  (t/is (= [true false true false]
           (sequence
            (comp (map sut/parse-passport)
                  (map (comp set keys))
                  (map (partial set/difference sut/required-fields))
                  (map (comp zero? count)))
            (str/split test-input #"\n\n")))))

(t/deftest part-1
  (t/is (= 200 (sut/part-1))))

(t/deftest part-2
  (t/is (= 116 (sut/part-2))))
