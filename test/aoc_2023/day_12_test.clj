(ns aoc-2023.day-12-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-12 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def example-data (str/split
                   ""
                   #"\n"))

(deftest compatible-test
  (testing "compatibility of spring arrangements with partial condition reports"
    (is (compatible?))))
