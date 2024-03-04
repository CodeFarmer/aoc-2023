(ns aoc-2023.day-2-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-2 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(deftest tok2pair-test
  (testing "Can parse draw sections"
    (is (= ["red" 1] (tok2pair "1 red")))))

(deftest get-numbers-test
  (testing "Extracting counts from a single draw"
    (is (= {"red" 1 "green" 2 "blue" 6}
           (draw-counts "1 red, 2 green, 6 blue")))))

(comment 
  (deftest combined-numbers-test
    (testing "Extracting counts from a play and combining them"
      (is (= {"blue" 9
              "red" 5
              "green" 4}
             (count-draws "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))))))

(deftest possibility-test
  (testing "a game string (without ID) can be checked against the bag's contents"
    (let [known-bag {"red" 12 "green" 13 "blue" 14}]
      (is (possible? known-bag "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))
      (is (possible? known-bag "1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"))
      (is (possible? known-bag "6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))
      (is (not (possible? known-bag "8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")))
      (is (not (possible? known-bag "1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"))))))

(def sample-data
  (str/split "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
             #"\n"))

(deftest id-and-line-test
  (is (= [5 "6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]
         (id-and-line "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))))

(deftest find-possible-draws-test
  (testing "For a series of plays, the IDs of the possible ones are returned"
    (let [known-bag {"red" 12 "green" 13 "blue" 14}]
      (is (= [1 2 5]
             (possible-draws known-bag sample-data))))))

(def input-data
  (str/split
   (slurp "aoc-2023-inputs/input-2.txt")
   #"\n"))

(deftest part-1-test
  (let [known-bag {"red" 12 "green" 13 "blue" 14}]
       (= 2278 (reduce + (possible-draws known-bag input-data)))))

;; Part 2

(deftest minimum-bag-test
  (testing "minumum cube counts to make a particular game possible"
    (is (= {"red" 4 "green" 2 "blue" 6}
           (minimum-counts [{"blue" 3 "red" 4}
                            {"red" 1 "green" 2 "blue" 6}
                            {"green" 2}])))))

(deftest power-test
  (is (= 48 (power (minimum-counts [{"blue" 3 "red" 4}
                            {"red" 1 "green" 2 "blue" 6}
                            {"green" 2}])))))

(deftest part-2-test
  (is (= 67953
         ;; sure it's horrible but it was fast
         ;; TODO is this one of those transducer thingies?
         (reduce + (map power (map minimum-counts-for-line (map second (map id-and-line input-data))))))))
