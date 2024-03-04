(ns aoc-2023.day-5-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-5 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data (str/split
                   "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
                   #"\n\n"))

(deftest map-creation-test
  (testing "map creation from a substring"
    (let [map-string "seed-to-soil map:
50 98 2
52 50 48"]
      (is (= {50 [98 2]
              52 [50 48]}
             (parse-map map-string))))))

(deftest data-parsing-test
  (testing "extraction of seeds and maps from a string"
    (let [parsed-data (parse-almanac sample-data)]
      (is (= [79 14 55 13]
             (:seeds parsed-data)))
      (is (= 7 (count (:maps parsed-data)))))))

(deftest map-location-test
  (let [soil-map {50 [98 2]
                  52 [50 48]}]
    (is (= 50 (find-destination soil-map 98)))
    (is (= 51 (find-destination soil-map 99)))
    (is (= 81 (find-destination soil-map 79)))
    (is (= 14 (find-destination soil-map 14)))
    (is (= 57 (find-destination soil-map 55)))
    (is (= 13 (find-destination soil-map 13)))))

(deftest walking-maps-test
  (let [maps (:maps (parse-almanac sample-data))]
    (is (= 43 (walk-maps 14 maps)))
    (is (= 82 (walk-maps 79 maps)))
    (is (= 86 (walk-maps 55 maps)))
    (is (= 35 (walk-maps 13 maps)))))

(def input-data
  (str/split (slurp "aoc-2023-inputs/input-5.txt") #"\n\n"))

(deftest part-1-test
  (let [{:keys [seeds maps]} (parse-almanac input-data)]
    (is (= 178159714
           (reduce min (map #(walk-maps % maps) seeds))))))

;; part 2

(deftest partition-ranges-test
  (testing "a range can be split into portions less than, inside and greater than another range"
    
    (is (= [[] [20 20] []]      
           (partition-range [20 20] [10 40]))
        "A wholly contained range should be in the middle")
    (is (= [[] [] [20 20]]      
           (partition-range [20 20] [10 10]))
        "A range entirely after the partitioner should be at the end")
    (is (= [[10 10] [] []]      
           (partition-range [10 10] [20 10]))
        "A range entirely before the partitioner should be at the beginning")
    (is (= [[20 10] [30 10] []]      
           (partition-range [20 20] [30 10]))
        "A range overlapping at the start should be in the first two partitions")
    (is (= [[] [20 10] [30 10]]      
           (partition-range [20 20] [20 10]))
        "A range overlapping at the end should be in the second two partitions")
    (is (= [[10 10] [20 10] [30 10]]      
           (partition-range [10 30] [20 10]))
        "A completely overlapping range should be in three partitions")
))


(deftest destinations-and-remainders-test
  (is (= (destinations-and-remainders [90 2] 50 [90 5])
         [[50 2] [] []])
      (= (destinations-and-remainders [90 5] 50 [90 2])
         [[50 2] [] [92 3]])))


(deftest ranges-through-map-test
  (let [seed-to-soil-map {50 [98 2] ; [97 1] [100 2] | [50 2]
                          52 [50 48]}] ; [
    (is (= [[50 1] [99 1]]
           (ranges-through-map [[97 2]] seed-to-soil-map)))
 
    (is (= [[50 2] [99 1] [100 2]]
           (ranges-through-map [[97 5]] seed-to-soil-map)))))

(deftest walk-maps-ranges-test
  (let [{:keys [seeds maps]} (parse-almanac sample-data)]
    (is (= 46
           (reduce min (map first (walk-maps-ranges (partition 2 seeds) maps)))))))

(deftest part-2-test
  (let [{:keys [seeds maps]} (parse-almanac input-data)]
    (is (= 100165128
           (reduce min (map first (walk-maps-ranges (partition 2 seeds) maps)))))))
