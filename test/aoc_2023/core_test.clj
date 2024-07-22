(ns aoc-2023.core-test
  (:require [clojure.test :refer :all]
            [aoc-2023.core :refer :all]))

(deftest minverse-test
  (is (= {} (minverse {})))
  (is (= {:b :a} (minverse {:a :b})))
  (is (= {[1 2] :a
          [2 3] :b} (minverse {:a [1 2] :b [2 3]}))))


(deftest map-rotate-test
  (testing "rotation clockwise"
    (is (= [] (tmap-rotate-right [])))
    (is (= ["1"] (tmap-rotate-right ["1"])))
    (is (= ["1"
            "2"] (tmap-rotate-right ["12"])))
    (is (= ["21" (tmap-rotate-right ["1"
                                     "2"])]))
    
    (is (= ["741"
            "852"
            "963"]
           (tmap-rotate-right ["123"
                               "456"
                               "789"]))))
  (testing "rotation counterclockwise"
    (is (= ["369"
            "258"
            "147"]
           (tmap-rotate-left ["123"
                              "456"
                              "789"])))))


(deftest cycle-finding-test
  (is (= nil  (find-cycle [1 2 3 4 5]))
      "No cycle found")
  (is (= [1] (find-cycle [1 1 1 1])))

  (is (= [1 2] (find-cycle [1 2 1 2]))
      "Cycles at the beginning should be found")
  (is (= [2 3 4] (find-cycle [1 2 3 4 2 3 4 2 3 4]))
      "Cycles not at the beginning should be found"))
