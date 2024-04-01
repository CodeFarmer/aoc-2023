(ns aoc-2023.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-12 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(deftest arrangement-test
  (testing "arrangements of one group"
    (is (= [] (arrangements 3 [4]))
        "There are no arrangements when the run length is longer than the available space")
    (is (= ["..."] (arrangements 3 []))
        "There is one blank arrangement when there are no run lengths")
    (is (= ["###"] (arrangements 3 [3]))
        "There is one arrangement when the only run length is the same as the available space")
    (is (= ["###." ".###"] (arrangements 4 [3]))
        "There are two arrangements with a single one-space gap")
    
    (is (= ["#.#"] (arrangements 3 [1 1]))
        "There is one arrangement of two unit runs")
    (is (= ["#.#." "#..#" ".#.#"] (arrangements 4 [1 1]))
        "There are thhree arrangement of two unit runs in four spaces")
    
    (is (= ["#.##." "#..##" ".#.##"] (arrangements 5 [1 2])))
    (is (= ["#.##.." "#..##." "#...##" ".#.##." ".#..##" "..#.##"] (arrangements 6 [1 2])))))


(deftest input-parsing-test
  (is (= ["#.#.###" [1 1 3]] (parse-line "#.#.### 1,1,3"))))

(deftest compatibility-test
  (is (compatible? "???.###" "#.#.###"))
  (is (not (compatible? "???.###" "#..####"))))

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

(defn total-compatibles [lines]
  (reduce (fn [a [pattern runs]] (+ a (count-compatibles pattern runs)))
          0
          lines))

(deftest sample-test
  (is (= 21 (total-compatibles sample-data))))

(deftest part-1-test
  (is (= 7725 (total-compatibles input-data))))
