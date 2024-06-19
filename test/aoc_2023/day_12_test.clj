(ns aoc-2023.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-12 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(deftest arrangement-derivation-test
  (testing "For debugging purposes, the list of runs can be derived from an arrangement string"
    (is (= [1 1 3] (-derive-arrangements [] 0 "#.#..###")))))

(deftest input-parsing-test
  (is (= ["#.#.###" [1 1 3]] (parse-line "#.#.### 1,1,3"))))

(deftest compatibility-counting-test
  (is (= 4
         (count-compatibles ".??..??...?##." [1 1 3])))
  (is (= 10
         (count-compatibles "?###????????" [3 2 1]))))

(def input-data
  
  (map parse-line
       (str/split
        (slurp "aoc-2023-inputs/input-12.txt")
        #"\n")))

(def sample-data
  (map parse-line 
       (str/split "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"
                  #"\n")))

(comment 
  (deftest sample-test
    (is (= 21 (total-compatibles sample-data)))))

(comment 
  (deftest part-1-test
    (is (= 7633 (total-compatibles input-data)))))
