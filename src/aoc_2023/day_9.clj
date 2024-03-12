(ns aoc-2023.day-9
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn parse-int-line [astr]
  (intify-seq (re-seq #"\d+" astr)))

(defn diff-seq [num-seq]
  (map (fn [[a b]] (- b a)) (partition 2 1 num-seq)))

(defn repeated-diff
  ([num-seq]
   (repeated-diff [] num-seq))
  ([acc num-seq]
   (if (every? zero? num-seq)
     (conj acc num-seq)
     (recur (conj acc num-seq) (diff-seq num-seq)))))
