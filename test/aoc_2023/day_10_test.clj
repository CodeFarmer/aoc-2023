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
  (is (= 6697 (/ (count (walk-loop input-map)) 2))))

(def bigger-loop-map
  (str/split "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."
             #"\n"))


(def third-loop-map
  (str/split ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."
             #"\n"))

(def junk-tiles-map
  (str/split "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"
             #"\n"))

(deftest tile-guessing-test
  (testing "Start tiles have their correct tile character deduced"
    (is (= \F (guess-start-tile third-loop-map)))
    (is (= \7 (guess-start-tile junk-tiles-map)))
    (is (= \F (guess-start-tile bigger-loop-map)))
    (is (= \| (guess-start-tile ["F"
                                 "S"
                                 "L"])))))

(deftest tile-correction-test
  (is (= (str/split
   "..F7.
.FJ|.
FJ.L7
|F--J
LJ..."
   #"\n")
         (correct-start-tile other-loop-map))))

;; showing why just counting tiles with verticals in it doesn't work
(def tricky-map
  [".F----7."
   ".|....|." ;; this has four inside
   ".L-7..|." ;; this has two inside
   "...L--J." ;; and this has none
   ])

(def squishy-map
  (str/split 
   "..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
.........."
   #"\n"))

(deftest counting-enclosed-tiles-test

  (let [loop (walk-loop tricky-map [1 0])]
    (is (= 0 (count-row-enclosed-tiles (first tricky-map) 0 loop)))
    (is (= 4 (count-row-enclosed-tiles (second tricky-map) 1 loop))))

  (let [loop (walk-loop bigger-loop-map)]
    (is (= 0 (count-row-enclosed-tiles (first bigger-loop-map) 0 loop)))
    (is (= 4 (count-row-enclosed-tiles (nth bigger-loop-map 6) 6 loop))))

  (let [loop (walk-loop squishy-map)]
    (is (= 0 (count-row-enclosed-tiles (first squishy-map) 0 loop)))
    (is (= 4 (count-row-enclosed-tiles (nth squishy-map 6) 6 loop))))

  (let [loop (walk-loop squishy-map)
        map' (correct-start-tile squishy-map)]
    (is (= 4 (count-enclosed-tiles map' loop)))))

(deftest part-2-test
  (let [loop (walk-loop input-map)
        map' (correct-start-tile input-map)]
    (is (= 423 (count-enclosed-tiles map' loop)))))

;; end solution ;; end solution ;; end solution ;; end solution
;; and now, the stuff that didn't work

(deftest edges-test
  (testing "the set of edge squares of a tube map are found"
    (is (= #{[0 0] [1 0] [2 0] [3 0] [4 0]
             [0 1]                   [4 1]
             [0 2]                   [4 2]
             [0 3]                   [4 3]
             [0 4] [1 4] [2 4] [3 4] [4 4]}
           (find-edges square-loop-map)))))

(deftest flood-seeds-test
  (testing "squares on the edges that are definitely outside the loop are found"
    (is (= #{[0 0] [1 0]              [4 0]
             [0 1]                    [4 1]



             [2 4] [3 4] [4 4]}
           (find-flood-seeds other-loop-map (walk-loop other-loop-map)))
        )))

(deftest flood-fill-test
  (testing "Starting from the edges, squares not contained by a loop are filled correctly"
    (let [loop (walk-loop bigger-loop-map)
          filled (flood-fill bigger-loop-map
                             loop)]
      (is (contains? filled [5 3])
          "flood fill should eventually reach the interior")
      (is (not (contains? filled [3 6]))
          "flood fill should not reach the blank squares inside the loop"))))



(comment "This is where I got to before I realised that flood-fill won't work"
         (deftest counting-enclosed-tiles-test
           (testing "enclosed tiles are counted correctly without junk tiles present"
             (let [loop (walk-loop third-loop-map)
                   filled (flood-fill third-loop-map
                                      loop)]
               (println "loop:")
               (println (show-square-str (into [] third-loop-map) loop))
               (println)
               (println "filled:")
               (println (show-square-str (into [] third-loop-map) filled))
               
               (is (= 8 (- (* (count third-loop-map)
                              (count (first third-loop-map)))
                           (+ (count loop)
                              (count filled)))))))))
