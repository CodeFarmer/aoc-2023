(ns aoc-2023.day-8-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-8 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str])
  (:import (java.math BigInteger)))

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

(def input-data (slurp "aoc-2023-inputs/input-8.txt"))
(deftest part-1-test
  (let [[directions graph] (parse-data-string input-data)]
    (is (= 19637 (count-steps directions graph "AAA" "ZZZ")))))

;; part 2

(def sample-data-2 "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)")

(deftest thunk-test
  (let [[directions graph] (parse-data-string sample-data-2)]
    (is (= ["11B" "22B"] (map #(next-move graph \L %) ["11A" "22A"])))))

(deftest count-while-not-all-terminated-test
  (let [[directions graph] (parse-data-string sample-data-2)]
    (is (= 6 (count-steps-until-all directions graph ["11A" "22A"] #(.endsWith % "Z"))))))

(deftest convenient-graph-shape-test
  (let [[directions graph] (parse-data-string input-data)
        start-nodes (filter ends-with-a? (keys graph))
        initial-probes (map #(probe-cycles 0 directions directions graph % ends-with-z?) start-nodes)
        subsequent-probes (map #(probe-cycles 0 directions directions graph % ends-with-z?) (map second initial-probes))]
    
    (testing "the length of each cycle from the end node to itself is the same as the length of the journey to get to it from a start node, and is also always a multiple of the length of the LR instructions - which means the LCM of the initial journeys is the first time they will all terminate"
      (is (every? empty? (map last initial-probes))
          "Each initial journey should empty the instruction list exactly")
      (is (= initial-probes subsequent-probes)
          "Each initial journey and loop should end at the same node and take the same number of steps"))

    (testing "LCM of the initial journey lengths is the number of moves taken to reach simultaneous termination"
      (is (= 8811050362409 (->> initial-probes
                                (map first)
                                (map #(BigInteger. (str %)))
                                (reduce lcm)))))))

