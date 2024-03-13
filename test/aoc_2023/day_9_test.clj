(ns aoc-2023.day-9-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-9 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(def sample-data
  
  (map parse-int-line
       (str/split "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"
                  #"\n")))

(deftest diff-seq-test
  (testing "the differences between pairs of numbres are found"
    (is (= [3 3 3 3 3]
           (diff-seq [0 3 6 9 12 15])))))

;; note this is upside-down compared to the example, for easier traversal
(deftest repeated-diffing-test
  (is (= [[  0 0 0 0]
          [ 3 3 3 3  3]
          [0 3 6 9 12 15]]
         (repeated-diff [0 3 6 9 12 15]))))

(deftest prediction-test
  (testing "Given a sequentially-subtracted sequence of number sequences, the correct next number is predicted for the last sequence"
    (is (= 18 (predict-next (repeated-diff [0 3 6 9 12 15]))))
    (is (= 28 (predict-next (repeated-diff [1 3 6 10 15 21]))))
    (is (= 68 (predict-next (repeated-diff [10 13 16 21 30 45]))))))

(def input-data
  (map parse-int-line
       (str/split (slurp "aoc-2023-inputs/input-9.txt")
                  #"\n")))

(deftest part-1-test
  (is (= 1581679977 (reduce + (->> input-data
                                   (map repeated-diff)
                                   (map predict-next))))))
