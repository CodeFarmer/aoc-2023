(ns aoc-2023.day-8-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-8 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")


(deftest line-parsing-test
  (testing "data lines can be parsed into three node names"
    (is (= ["AAA" "BBB" "CCC"] (parse-node-line "AAA = (BBB, CCC)")))))


(deftest data-parsing-test
  (testing "Data is parsed into a string containing a sequence of steps, and a map of nodes with pairs of connections"
    (let [[directions graph] (parse-data-string sample-data)]
      (is (= "RL" directions))
      (is (= ["BBB" "CCC"] (graph "AAA"))
          "Graph should return pairs of strings as its values when asked for a node name"))))


(deftest count-steps-test

  (testing "Steps required to reach ZZZ from AAA following a direction string are counted correctly"
    (let [[directions graph] (parse-data-string sample-data)]
      (is (= 2 (count-steps directions graph "AAA" "ZZZ")))))
  
  (testing "When you run out of steps, start again"
    (let [[directions graph] (parse-data-string "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")]
      (is (= 6 (count-steps directions graph "AAA" "ZZZ"))))))

(deftest part-1-test
  (let [input-data (slurp "aoc-2023-inputs/input-8.txt")
        [directions graph] (parse-data-string input-data)]
    (is (= 0 (count-steps directions graph "AAA" "ZZZ")))))
