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

(deftest run-possibility-test
  (testing "A string has a single run of broken springs, starting at a specific index"
    (is (run-possible-at? "#"    1 0))
    (is (run-possible-at? "#.."  1 0))
    (is (run-possible-at? "##."  2 0))
    (is (run-possible-at? ".#"   1 1))
    (is (not (run-possible-at? "."      1 0)))
    (is (not (run-possible-at? "##"     1 0 true))
        "A run of length 1 cannot end adjacent to a broken spring")
    (is (not (run-possible-at? "#."     2 0))))
  (testing "A string is compatible with having a single run of broken springs, starting at a specific index"
    (is (run-possible-at? "?"  1 0))
    (is (run-possible-at? "?#" 1 1))
    (is (run-possible-at? "#?" 1 0 true)
        "A run of length 1 can end adjacent to a spring of unknown status")))

(deftest just-the-broken-test

  (is (run-possible-at? "?.?" 1 2))
  (is (= 2
         (count-compatibles "?.?" [1]))))

(deftest compatibility-counting-test

  ;; various ways to arrange one or no runs
  (is (= 1
         (count-compatibles "..." [])))

  (is (= 0
         (count-compatibles "#.." [])))

  (is (= 1
         (count-compatibles "#.." [1])))

  (is (= 2
         (count-compatibles "?.?" [1])))

  (is (= 1
         (count-compatibles "?.#" [1])))

  (is (= 2
         (count-compatibles "??#?.." [2])))

  ;; two or more

  (is (= 1
         (count-compatibles "#.#" [1 1])))
  
  (comment this is where the actual examples start)
 
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
 
(deftest sample-test
  (is (= 21 (total-compatibles sample-data))))

(deftest part-1-test
  (is (= 7633 (total-compatibles input-data))))

(deftest expanded-sample-test
  (is (= 525152 (total-expanded-compatibles sample-data))))

(deftest part-2-test
  (is (= 23903579139437 (total-expanded-compatibles input-data))))

(comment
  ;TODO: consider FSA?
  )
