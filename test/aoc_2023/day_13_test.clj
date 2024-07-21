(ns aoc-2023.day-13-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-13 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

; I have this dumb idea that I'll be able to use the same symmetry
; code for horizontal and vertical by rotating the maps

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


(deftest smudge-test
  (is (= #{["##"
            "##"]
           [".."
            "##"]
           [".#"
            ".#"]
           [".#"
            "#."]}
         (into #{}
               (generate-smudges [".#"
                                  "##"])))))

(deftest smudge-finding-test
  (let [smudged (smudge-at sample-0 0 0)]
    (comment 
      (println (symmetry-lines sample-0))
      (println (symmetry-lines smudged))
      (println (str/join "\n" smudged)))

    (is (= [5]
           (all-vertical-symmetry-lines smudged)))
    (is (= [3]
           (all-horizontal-symmetry-lines smudged))))

  (is (= [0 3] (find-smudged-reflection sample-0)))
  (is (= [0 1] (find-smudged-reflection sample-1))))

(def smudge-bug-example
  (str/split
   "##..#..#.
..###..##
###.###..
###......
....#..#.
...#.##.#
.....##..
##...##..
##.#....#
" #"\n"))

;; [1 0] [nil nil]

;; found the bug: if there is a new symmetry line that comes after the old one, it doesn't get seen

(deftest smudge-bug-test
  (is (= [6 0] (find-smudged-reflection smudge-bug-example))))


(deftest smudged-scoring-test
  (is (= 300 (smudged-score sample-0)))
  (is (= 100 (smudged-score sample-1))))

(deftest part-2-test
  (is (= 31539 (reduce + (map smudged-score input-data)))))
