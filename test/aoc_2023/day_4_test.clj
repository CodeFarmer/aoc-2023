(ns aoc-2023.day-4-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-4 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
             #"\n"))

(deftest card-parsing-test
  (is (= [[41 48 83 86 17] [83 86 6 31 17 9 48 53]]
         (parse-cards (first sample-data)))))

(deftest win-counting-test
  (is (= [83 86 17 48] (winning-numbers [41 48 83 86 17] [83 86 6 31 17 9 48 53]))))

(deftest scoring-test
  (is (= 8
         (score-card [41 48 83 86 17] [83 86 6 31 17 9 48 53])))
  (is (= 13 (reduce + (->> sample-data
                           (map parse-cards)
                           (map #(apply score-card %)))))))

(def input-data
  (str/split (slurp "input-4.txt")
             #"\n"))

(deftest part-1-test
  (is (= 23678
         (reduce + (->> input-data
                        (map parse-cards)
                        (map #(apply score-card %)))))))

;; part 2

(def sample-data-with-counts
  (->> sample-data
       (map parse-cards)
       (map (fn [e] [e 1]))))

(deftest increment-n-counts-test
  (is (= [2 2 2 1 1 1]
         (map second (increment-n-counts 3 sample-data-with-counts)))))

(deftest cards-as-prizes-test
  (testing "starting with one of each card, the right number of cards are added with wins"
    (is (= [1 2 4 8 14 1]
           (map second (eventual-counts sample-data-with-counts))))
    (is (= 30
           (reduce + (map second (eventual-counts sample-data-with-counts)))))))

(deftest part-2-test
  (is (= 15455663
         (reduce +
                 (map second
                      (eventual-counts (->> input-data
                                            (map parse-cards)
                                            (map (fn [e] [e 1])))))))))
