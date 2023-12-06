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

(deftest distance-test
  (testing "distances for various hold and race times are calculated"
    (is (=  0 (distance-travelled 0 7)))
    (is (=  6 (distance-travelled 1 7)))
    (is (= 10 (distance-travelled 2 7)))
    (is (=  0 (distance-travelled 7 7)))))

(deftest winning-ranges-test
  (testing "minimum and maximum hold times for a race to beat a given distance are calculated"
    (is (= [ 2  5] (winning-range  7   9)))
    (is (= [ 4 11] (winning-range 15  40)))
    (is (= [11 19] (winning-range 30 200)))))

(deftest total-ways-test
  (testing "The total number of ways to win a series of races multiply correctly"
    (let [races (zipmap (:time example-data)
                        (:distance example-data))]
      (is (= 288
             (->> races 
                  (map #(apply winning-range %))
                  (map #(apply - %))
                  (map dec)
                  (map abs)
                  (reduce *)))))))

(deftest part-1-test
  (let [races (zipmap (:time input-data)
                      (:distance input-data))]
      (is (= 2374848
             (->> races 
                  (map #(apply winning-range %))
                  (map #(apply - %))
                  (map dec)
                  (map abs)
                  (reduce *))))))

;; part 2

(deftest part-2-test
  (is (= 39132886
         (abs (dec (apply - (winning-range 55999793 401148522741405)))))))
