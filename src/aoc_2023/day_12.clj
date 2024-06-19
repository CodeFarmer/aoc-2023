(ns aoc-2023.day-12
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


;; for debugging
(defn -derive-arrangements [acc n astr]
  (if (empty? astr)
    (if (= 0 n)
      acc
      (conj acc n))
    (let [c (first astr)]
      (cond (= c \#) (recur acc (inc n) (rest astr))
            (> n 0)  (recur (conj acc n) 0 (rest astr))
            :default (recur acc 0 (rest astr))))))


(defn parse-line [astr]
  (let [[rec runstr] (str/split astr #" ")
        runs (intify-seq (str/split runstr #","))]
    [rec runs]))
