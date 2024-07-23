(ns aoc-2023.day-16-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-16 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split
   ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."
   #"\n"))

;; don't ask
(deftest sample-data-test
  (is (every? #(= 10 (count %)) sample-data)
      "Sanity check: the escape characters end up with each row the same length"))

(def input-data
  (lines-as-vector "aoc-2023-inputs/input-16.txt"))

(deftest next-direction-finding-test
  (testing "Given a tile and an input direction, the next directions are found"
    (is (= [:right] (next-dirs :right \.)))
    (is (= [:down] (next-dirs :right \\)))
    (is (= [:up] (next-dirs :right \/)))
    (is (= [:right] (next-dirs :right \-))
        "hitting a horizontal splitter horizontally continues straight")
    (is (= [:left :right] (next-dirs :down \-))
        "hitting a horizontal splitter horizontally splits")
    (is (= [:up] (next-dirs :up \|))
        "hitting a vertical splitter vertically continues straight")
    (is (= [:up :down] (next-dirs :left \|))
        "hitting a vertical splitter horizontally splits")))

(deftest next-square-finding-test
  (testing "given a direction, a point and a tilemap, the next squares and directions are found"
    (is (= [[:right [1 0]]] (next-squares :right [0 0] sample-data)))
    (is (= [] (next-squares :right [9 0] sample-data))
        "can't go off the map to the right")
    (is (= [[:left [0 7]] [:right [2 7]]] (next-squares :down [1 7] sample-data))
        "horizontal splitter works as advertised")
    (is (= [[:down [1 1]]] (next-squares :right [1 0] sample-data))
        "vertical splitter at top edge only goes down")))

(def simple-map-0
  ["..."
   "./."
   "..."])

(def simple-map-1
  ["./."
   ".|."
   "..."])

(deftest map-traversal-test
  (is (= 3 (count (energized-tiles :right [0 1] simple-map-0))))
  (is (= 5 (count (energized-tiles :right [0 1] simple-map-1)))
      "Maps with splitters have more energized tiles")
  (is (= 46 (count (energized-tiles :right [0 0] sample-data)))))

(deftest part-1-test
  (is (= 6514 (count (energized-tiles :right [0 0] input-data)))))

(deftest starting-points-test
  (is (= [[:down  [0 0]] [:down  [1 0]]
          [:right [0 0]] [:right [0 1]]
          [:up    [0 1]] [:up    [1 1]]
          [:left  [1 0]] [:left  [1 1]]]
         (starting-points [".."
                           ".."]))))

(deftest maximising-test
  (is (= 51 (apply max (pmap (fn [[dir point]]
                              (count (energized-tiles dir point sample-data)))
                            (starting-points sample-data))))))

(deftest part-2-test
  (is (= 8089 (apply max (pmap (fn [[dir point]]
                                (count (energized-tiles dir point input-data)))
                              (starting-points input-data))))))
