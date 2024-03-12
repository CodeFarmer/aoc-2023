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

(deftest repeated-diffing-test
  (is (= [[0 3 6 9 12 15]
          [ 3 3 3 3  3]
          [  0 0 0 0]]
         (repeated-diff [0 3 6 9 12 15]))))
