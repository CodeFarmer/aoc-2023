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

(def simple-square-data
  {[0 0] {:prev nil   :cost 0}
   [1 0] {:prev [0 0] :cost 1}
   [1 1] {:prev [1 0] :cost 2}
   [1 2] {:prev [1 1] :cost 3}
   [2 2] {:prev [1 2] :cost 4}
   [3 2] {:prev [2 2] :cost 5}})

(deftest unwinding-test
  (is (= [[0 0] [1 0] [1 1] [1 2] [2 2] [3 2]]
         (unwind-path simple-square-data [3 2]))))

(deftest journey-cost-test
  (is (= 6 (cost-from simple-square-data simple-city [1 2] [0 2]))))

(deftest lower-paths-finding-test
  (let [square-data {[0 0] {:prev nil   :cost 0}
                     [1 0] {:prev [0 0] :cost 1}
                     [1 1] {:prev [1 0] :cost 5}}]
    (is (= {[2 0] {:prev [1 0] :cost 4} ;; unseen node
            [1 1] {:prev [1 0] :cost 2} ;; shorter path
            }
           (find-lower-paths square-data simple-city [1 0] (tmap-find-neighbours [1 0] simple-city))))))

(deftest path-finding-test
  (is (= [[0 0]]
         (find-lowest-path ["5"])))
  
  (is (= [[0 0] [0 1]] (find-lowest-path ["5"
                                          "6"])))
  
  (comment 
    (is (= [[0 0] [1 0] [1 1] [1 2] [2 2] [3 2]]
           (find-lowest-path simple-city)))))
