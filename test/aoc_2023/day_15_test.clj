(ns aoc-2023.day-15-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-15 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def sample-data
  (str/split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
             #","))

(deftest holiday-hash-test
  (is (= 0 (hhash "")))
  (is (= (hhash "HASH") 52))
  (is (= 1320
         (reduce + (map hhash sample-data)))))

(def input-data
  (str/split (slurp "aoc-2023-inputs/input-15.txt")
             #","))

(deftest part-1-test
  (is (= 504036
         (reduce + (map hhash input-data)))))

;; part 2

;; (array-map) has exactly the key ordering properties the text calls
;; for
(deftest lens-placing-test
  (let [boxes (into [] (repeat 4 (array-map)))]
    (is (= [{"rn" 1}
            {}
            {}
            {}]
           (process-command boxes "rn=1"))))
 
  (let [boxes [{"rn" 1 "cm" 2}
               {"qp" 3}]]
    (is (= [{"rn" 1 "cm" 2}
            {}]
           (process-command boxes "qp-"))))

  (let [boxes (into [] (repeat 4 (array-map)))]
    (is (= [{"rn" 1 "cm" 2}
            {}
            {}
            {"ot" 7 "ab" 5 "pc" 6}]

           (reduce process-command boxes sample-data)))))

(deftest focal-power-test
  (is (= 5 (box-score 0 {"rn" 1 "cm" 2})))
  (let [boxes (into [] (repeat 4 (array-map)))
        final-boxes (reduce process-command boxes sample-data)]
    (is (= 145
           (->> (range 0 (count final-boxes))
                (map #(box-score % (get final-boxes %)))
                (reduce +))))))

(deftest part-2-test
  (let [boxes (into [] (repeat 256 (array-map)))
        final-boxes (reduce process-command boxes input-data)]
    (is (= 295719
           (->> (range 0 (count final-boxes))
                (map #(box-score % (get final-boxes %)))
                (reduce +))))))
