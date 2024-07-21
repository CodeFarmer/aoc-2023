(ns aoc-2023.core-test
  (:require [clojure.test :refer :all]
            [aoc-2023.core :refer :all]))

(deftest minverse-test
  (is (= {} (minverse {})))
  (is (= {:b :a} (minverse {:a :b})))
  (is (= {[1 2] :a
          [2 3] :b} (minverse {:a [1 2] :b [2 3]}))))


(deftest map-rotate-test
  (is (= [] (map-rotate [])))
  (is (= ["1"] (map-rotate ["1"])))
  (is (= ["1"
          "2"] (map-rotate ["12"])))
  (is (= ["21" (map-rotate ["1"
                            "2"])]))
  
  (is (= ["741"
          "852"
          "963"]
         (map-rotate ["123"
                      "456"
                      "789"]))))
