(ns aoc-2023.day-4
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.math :as math]
            [clojure.pprint :as pp]))


;; Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
(defn parse-cards [astr]
  (let [[_ numstr] (str/split astr #":\s+")
        cards (str/split numstr #"\s+\|\s+")
        split-cards (map #(map parse-long (str/split % #"\s+")) cards)]
    split-cards))

;; this is one of those moments where Clojure is so nice
(defn winning-numbers [winners actuals]
  (filter (into #{} winners) actuals))

(defn score-card [winners actuals]
  (let [n (count (winning-numbers winners actuals))]
    (cond (= 0 n) 0
          (= 1 n) 1
          :default (int (math/pow 2 (dec n))))))

;; Part 2

(defn increment-n-counts
  ([n cards]
   (increment-n-counts n 1 cards))
  ([n score cards]
   (-> []
       (into (map (fn [[card c]] [card (+ score c)]) (take n cards)))
       (into (drop n cards)))))

(defn eventual-counts
  ([cards]
   (eventual-counts [] cards))
  ([acc cards]
   (if (empty? cards)
     acc
     (let [[card-pair mul] (first cards)]
       (recur (conj acc (first cards))
              (increment-n-counts
               (count (apply winning-numbers card-pair)) mul (rest cards)))))))
