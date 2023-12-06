(ns aoc-2023.day-6-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-6 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

;; Time:        55     99     97     93
;; Distance:   401   1485   2274   1405
(def input-data
  {:time     [ 55   99   97   93]
   :distance [401 1485 2274 1405]})

(def example-data
  {:time     [7 15  30]
   :distance [9 40 200]})

(deftest a-test
  (testing "failure"
    (is (= 0 1))))
