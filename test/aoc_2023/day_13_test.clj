(ns aoc-2023.day-13-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-13 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

; I have this dumb idea that I'll be able to use the same symmetry
; code for horizontal and vertical by rotating the maps

(deftest map-rotate-test
  (is (= [] (map-rotate [])))
  (is (= ["1"] (map-rotate ["1"])))
  (is (= ["1"
          "2"] (map-rotate ["12"])))
  (is (= ["21" (map-rotate ["1"
                            "2"])]))
  
  (is (= ["741"
          "852"
          "963"]
         (map-rotate ["123"
                      "456"
                      "789"]))))


(def sample-0
  (str/split
   "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."
   #"\n"))

(def sample-1
  (str/split
   "#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"
   #"\n"))

(deftest symmetrical-at-test
  (is (symmetrical-at? [2 2] 1))
  (is (not (symmetrical-at? [2 1] 1)))

  (testing "symmetry is assumed beyond the ends of the vector"
    (is (symmetrical-at? [1 2 3 4 4 3] 4))
    (is (symmetrical-at? [1 2 2 1 4 3] 2))
    (is (not (symmetrical-at? [1 2 2 1 4 3] 3))))

  (testing "symmetry data is understood"
    (is (symmetrical-at? sample-1 4)))

  (testing "symmetry data works by rotating"
    (is (symmetrical-at? (map-rotate sample-0) 5))))


(deftest symmetry-line-test
  (is (nil? (symmetry-line [])))
  (is (= 4 (symmetry-line [1 2 3 4 4 3])))

  (is (= 1 (symmetry-line [1 1 2 3 4 5])))

  (is (= 4 (symmetry-line sample-1)))
  (is (nil? (symmetry-line sample-0)))
  (is (= 5 (symmetry-line (map-rotate sample-0))))

  (testing "horizontal and vertical symmetry"
    (is (= 0 (horizontal-symmetry-line sample-0)))
    (is (= 5 (vertical-symmetry-line sample-0)))
    (is (= 4 (horizontal-symmetry-line sample-1)))
    (is (= 0 (vertical-symmetry-line sample-1)))))

(deftest scoring-test
  (is (= 5   (score sample-0)))
  (is (= 400 (score sample-1))))

(def input-data
  (map #(str/split % #"\n")
       (str/split 
        (slurp "aoc-2023-inputs/input-13.txt")
        #"\n\n")))

(deftest part-1-test
  (is (= 31739 (reduce + (map score input-data)))))
