(ns aoc-2023.day-1-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-1 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def example-data (str/split
                   "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
                   #"\n"))

(deftest first-last-test
  (testing "Finding the first and last digit in a line"
    (is (= [1 2]
           (first-and-last (first example-data))))))

(deftest nmake-number-test
  (testing "Turning a pair of digits into a number"
    (is (= 12
           (make-number [1 2])))))


(deftest sum-signals-test
  (testing "Given pairs of digits, find the sums of the numbers they make"
    (is (= 142
           (calibration-value example-data)))))

(def input-data
  (str/split
   (slurp "input-1.txt")
   #"\n"))

(deftest part-1-test
  (testing "Part one is answered correctly for the input data"
    (is (= 55130
           (calibration-value input-data)))))

;; Part 2

(deftest words-to-integers-test
  (testing "parsing integers from their names"
    (is (= 1 (wtoi "one")))
    (is (= 2 (wtoi "two")))))

(deftest words-or-ints-to-integers-test
  (testing "strings can be digits or words, parse them to integers"
    (is (= 7 (atoi "seven")))
    (is (= 5 (atoi "five")))
    (is (= 3 (atoi "3")))))

(def part-2-example-data
  (str/split 
   "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"
   #"\n"))

(deftest first-last-with-words-test
  (testing "Finding the first and last digit in a line, including word tokens"
    (is (= [2 9]
           (first-and-last-with-words (first part-2-example-data)))
        (= [7 6]
           (first-and-last-with-words (last part-2-example-data)))))
  (testing "a string with digit words sharing a letter should be separated correctly"
    (is (= [1 8]
           (first-and-last-with-words "oneight")))))


(deftest sum-signals-words-test
  (testing "Given pairs of digits including words, find the sums of the numbers they make"
    (is (= 281
           (calibration-value-with-words part-2-example-data)))))

(deftest part-2-test
  (testing "Part one is answered correctly for the input data"
    (is (= 54985
           (calibration-value-with-words input-data)))))
