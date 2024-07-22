(ns aoc-2023.day-14-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-14 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

;; Low-tech idea: all of this could be expressed in terms of rolling
;; things to the left, through rotation
(deftest rolling-left-test
  (is (= "...." (roll-left "....")))
  (is (= "O..." (roll-left "..O.")))
  (is (= "OO.." (roll-left ".OO.")))
  (is (= ".#O." (roll-left ".#.O"))
      "Fixed rocks should block rolling")
  (is (= ".#OO..#O." (roll-left ".#.O.O#.O"))
      "Fixed rocks should block rolling, redux")
  (is (= "OO." (roll-left "O.O"))
      "Rocks at the beginning of the string should not blow up"))

(def sample-0
  (str/split
   "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."
   #"\n"))

(def sample-1
  (str/split
   "OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...."
   #"\n"))

(deftest rolling-all-rocks-test
  (testing "By rotating a map to the left, rolling all its rocks to the left, and rotating it to the right again, you get the map with all its rocks rolled to the top"
    (is (= sample-1)
        (->> sample-0
             (tmap-rotate-left)
             (map roll-left)
             (tmap-rotate-right)))))

(deftest scoring-test
  (testing "By rotating a map to the left, rollings its rocks to the left, and scoring rocks by distance from the right, you get the score as though you had rolled up and scored by distance to the bottom")
  (is (= 136
         (->> sample-0
              (tmap-rotate-left)
              (map roll-left)
              (map score-rocks-distance-right)
              (reduce +)))))

(def input-data (lines-as-vector "aoc-2023-inputs/input-14.txt"))

(deftest part-1-test
  (is (= 106997
         (->> input-data
              ;; TODO DRY this up
              (tmap-rotate-left)
              (map roll-left)
              (map score-rocks-distance-right)
              (reduce +)))))

(def sample-2
  (str/split
   ".....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#...."
   #"\n"))


(deftest scoring-totals-test
  (is (= 136
         (total-score-rocks-distance-bottom sample-1))))

(deftest spin-cycle-test
  (is (= sample-2 (spin-cycle sample-0))))

;; this actually works by brute force (a few minutes) on a 17600K but
;; it's embarrassing
;; (first (drop (dec 1000000000) (iterate spin-cycle sample-0)))

;; This makes it very obvious there is a cycle
;; (take 20 (map total-score-rocks-distance-bottom (iterate spin-cycle sample-0)))
;; -> (104 87 69 69 69 65 64 65 63 68 69 69 65 64 65 63 68 69 69 65)

(deftest part-2-test
  (is (= 99641
         (total-score-rocks-distance-bottom (nth-with-cycles (iterate spin-cycle input-data) 1000000000)))))
