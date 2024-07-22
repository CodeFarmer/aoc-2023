(ns aoc-2023.day-15
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn hhash
  ([astr]
   (hhash 0 astr))
  ([acc astr]
   (cond (empty? astr) acc
         (= \ (first astr)) (recur acc (rest astr))
         :default (recur (-> acc
                             (+ (int (first astr)))
                             (* 17)
                             (rem 256))
                         (rest astr)))))
