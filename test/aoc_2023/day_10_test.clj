(ns aoc-2023.day-10-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-10 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(def square-loop-map
  (str/split
   ".....
.F-7.
.|.|.
.L-J.
....."
   #"\n"))

(deftest step-finding-test
  (testing "given a map and a coordinate pair, return the coordinates to which the point is connected by its pipe tile (if any)"
    (is (empty? (find-steps square-loop-map [3 0])))
    (is (= #{[2 1] [1 2]} (find-steps square-loop-map [1 1]))
        "F tile should return the tiles to the right and below")
    (is (= #{[3 1] [3 3]} (find-steps square-loop-map [3 2]))
        "| tile should return the tiles to the right and below")))

(deftest exit-guessing-test
         (testing "from surrounding tiles, guess the exactly two exits from a start tile"
      (is (= #{[1 2] [2 1]} (guess-exits square-loop-map [1 1])))))

(comment 
  (deftest loop-counting-test
    (testing "Loops are counted accurately"
      (is (= 8 (count-loop-steps square-loop-map [1 1]))))))

(deftest loop-walking-test
  (testing "It is possible to follow a loop of pipe and record the route taken"
    (is (= #{[1 2] [1 3] [2 3] [3 3] [3 2] [3 1] [2 1] [1 1]}
           (walk-loop square-loop-map [1 1])))))

(def other-loop-map
  (str/split
   "..F7.
.FJ|.
SJ.L7
|F--J
LJ..."
   #"\n"))

(deftest start-finding-test
  (is (= [0 2] (find-start other-loop-map))))

(def input-map
  (str/split (slurp "aoc-2023-inputs/input-10.txt")
             #"\n"))

(deftest part-1-test
  (is (= 6697 (/ (count (walk-loop input-map (find-start input-map))) 2))))

(deftest edges-test
  (testing "the set of edge squares of a tube map are found"
    (is (= #{[0 0] [1 0] [2 0] [3 0] [4 0]
             [0 1]                   [4 1]
             [0 2]                   [4 2]
             [0 3]                   [4 3]
             [0 4] [1 4] [2 4] [3 4] [4 4]}
           (find-edges square-loop-map)))))
