(ns aoc-2023.core-test
  (:require [clojure.test :refer :all]
            [aoc-2023.core :refer :all]))

(deftest minverse-test
  (is (= {} (minverse {})))
  (is (= {:b :a} (minverse {:a :b})))
  (is (= {[1 2] :a
          [2 3] :b} (minverse {:a [1 2] :b [2 3]}))))
