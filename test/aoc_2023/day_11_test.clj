(ns aoc-2023.day-11-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-11 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def example-data
  (str/split
   "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."
   #"\n"))

(deftest galaxy-finding-test
  
  (testing "Galaxies are found in the row"
    (is (= [[3 0]] (-find-row-galaxies 0 (first example-data)))))
  
  (testing "Galaxies are found in the starfield"
    (let [galaxies (find-galaxies example-data)]
      (is (= 9 (count galaxies)))
      (is (galaxies [7 1])
          "each galaxy should be in the returned set, as a [x y] point"))))

(deftest empty-finding-test

  (testing "rows without galaxies are identified"
    (is (= #{3 7} (find-empty-rows example-data))))

  (testing "columns without galaxie are identified"
    (is (= #{2 5 8} (find-empty-cols example-data)))))

;; sample-data with the empty columns and rows doubled
(def expanded-data
  (str/split
   "....#........
    .........#...
#............
.............
.............
........#....
.#...........
............#
.............
.............
.........#...
#....#......."
   #"\n"))

(deftest distance-counting-test
  (is (= 5 (distance [3 0] [7 1])))
  (is (= 6 (distance (find-empty-rows example-data)
                     (find-empty-cols example-data)
                     [3 0]
                     [7 1]))))

(deftest pairs-test
  (is (= 36 (count (pairs (find-galaxies example-data)))))
  (is (= 36 (count (pairs (find-galaxies expanded-data))))))

(deftest distance-sum-test
  (comment "this doesn't add up for some reason, is the expansion wrong?"
    (is (= 374 (distance-sum expanded-data))))
  
  (is (= 374 (distance-sum example-data (find-empty-rows example-data) (find-empty-cols example-data)))))

(def input-data
  (str/split
   (slurp "aoc-2023-inputs/input-11.txt")
   #"\n"))

(deftest part-1-test
  (is (= 9599070 (distance-sum input-data (find-empty-rows input-data) (find-empty-cols input-data)))))

(deftest part-2-test
  (is (= 842645913794 (distance-sum-scaled 1000000 input-data (find-empty-rows input-data) (find-empty-cols input-data)))))
