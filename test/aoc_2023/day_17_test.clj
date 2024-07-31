(ns aoc-2023.day-17-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-17 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split
"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"
   #"\n"))

(def simple-city
  ["1133"
   "3133"
   "3111"])

(deftest path-finding-test
  (is (= [[0 0] [0 1]] (find-lowest-path ["5"
                                          "6"])))
  (comment 
    (is (= [[0 0] [1 0] [1 1] [1 2] [2 2] [3 2]]
           (find-lowest-path simple-city)))))
