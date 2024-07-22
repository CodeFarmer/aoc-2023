(ns aoc-2023.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-15 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(deftest holiday-hash-test
  (is (= 0 (hhash "")))
  (is (= (hhash "HASH") 52))
  (let [sample-commands (str/split sample-data #",")]
    (is (= 1320
           (reduce + (map hhash sample-commands))))))

(def input-data (slurp "aoc-2023-inputs/input-15.txt"))

(deftest part-1-test
  (is (= 504036
         (reduce + (map hhash (str/split input-data #","))))))
