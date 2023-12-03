(ns aoc-2023.day-3-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-3 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split
   "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
   #"\n"))

(deftest number-string-finding-test
  (testing "Numbers strings and their start indices are found"
    (is (= [[0 "467"] [5 "114"]]
           (index-number-strings (first sample-data))))
    (is (= [[2 "345"]] (index-number-strings "..345")))))

(deftest symbol-finding-test
  (testing "Symbols are recognised"
    ;; symbol is a reserved word in Clojure
    (is (sigil? \*))
    (is (not (sigil? \.)))
    (is (not (sigil? \7)))))

(deftest part-number-finding-test
  (testing "part numbers are correctly located"
    (is (= [467 35 633 617 592 755 664 598]
           (part-numbers sample-data)))
    (is (= 4361 (reduce + (part-numbers sample-data))))))

(def input-data
  (str/split
   (slurp "input-3.txt")
   #"\n"))

(deftest part-1-test
  (is (= 509115 (reduce + (part-numbers input-data)))))

;;;; part 2

(deftest gear-finding-test
  (testing "find gears by returning their pairs of part numbers"
    (is (= [[467 35] [755 598]]
           (find-gear-parts sample-data)))))

(deftest ratio-finding-test
  (is (= 467835
         (reduce + (map #(apply (partial *) %) (find-gear-parts sample-data))))))

(deftest part-2-test
  (is (= 75220503
         (reduce + (map #(apply (partial *) %) (find-gear-parts input-data))))))
