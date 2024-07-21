(ns aoc-2023.day-11
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

;; starfield is a vector of row strings

(defn -find-row-galaxies [y astr]
  (for [x (range 0 (count astr))
        :when (= \# (.charAt astr x))]
    [x y]))

;; TODO this is potentially useful for core

(defn find-galaxies
  ([starfield]
   (find-galaxies #{} 0 starfield))
  ([acc y starfield]
   (if (empty? starfield) acc
       (recur (into acc (-find-row-galaxies y (first starfield)))
              (inc y)
              (rest starfield)))))

(defn find-empty-rows [starfield]
  (into #{}
        (filter #(= -1 (.indexOf (starfield %) "#"))
                (range 0 (count starfield)))))

(defn find-empty-cols [starfield]
  (into #{}
        (filter (fn [x] (not-any? #(= \# (.charAt % x)) starfield))
                (range 0 (count (first starfield))))))

(defn between? [start x end]
  (if (> end start)
    (< start x end)
    (< end x start)))

(defn distance
  ([[x0 y0] [x1 y1]]
   (distance #{} #{} [x0 y0] [x1 y1]))
  ([empty-rows empty-cols [x0 y0] [x1 y1]]
   (+ (abs (- x0 x1))
      (abs (- y0 y1))
      (count (filter #(between? x0 % x1) empty-cols))
      (count (filter #(between? y0 % y1) empty-rows)))))

(defn distance-scaled
  ([scale [x0 y0] [x1 y1]]
   (distance-scaled scale #{} #{} [x0 y0] [x1 y1]))
  ([scale empty-rows empty-cols [x0 y0] [x1 y1]]
   (+ (abs (- x0 x1))
      (abs (- y0 y1))
      (* (dec scale) (count (filter #(between? x0 % x1) empty-cols)))
      (* (dec scale) (count (filter #(between? y0 % y1) empty-rows))))))

(defn pairs [aset]
  (comb/combinations aset 2))

(defn distance-sum-scaled
  ([scale starfield]
   (distance-sum-scaled scale starfield #{} #{}))
  ([scale starfield empty-rows empty-cols]
   (->> starfield
        (find-galaxies)
        (pairs)
        (map #(apply (partial distance-scaled scale empty-rows empty-cols) %))
        (reduce + 0))))

(defn distance-sum
  ([starfield]
   (distance-sum starfield #{} #{}))
  ([starfield empty-rows empty-cols]
   (distance-sum-scaled 2 starfield empty-rows empty-cols)))

