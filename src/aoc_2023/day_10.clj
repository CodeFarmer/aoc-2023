(ns aoc-2023.day-10
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def exit-offsets
  {
   \F #{[ 1  0] [ 0  1]}
   \- #{[-1  0] [ 1  0]}
   \| #{[ 0 -1] [ 0  1]}
   \7 #{[-1  0] [ 0  1]}
   \L #{[ 0 -1] [ 1  0]}
   \J #{[-1  0] [ 0 -1]}
   })

(defn get-tile [pipe-map [x y]]
  (get-in pipe-map [y x]))

(defn find-steps [pipe-map [x y]]
  (let [tile (get-tile pipe-map [x y])]
    (into #{}
          (map (fn [[dx dy]] [(+ x dx) (+ y dy)])
               (get exit-offsets tile #{})))))

(defn guess-exits [pipe-map [x y]]

  (into #{}
        (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
              :let [x' (+ x dx) y' (+ y dy)]
              :when (contains? (find-steps pipe-map [x' y']) [x y])]
          [x' y'])))

(defn -walk-loop
  [walked pipe-map current-point]
  (let [exits (if (= \S (get-tile pipe-map current-point)) (guess-exits pipe-map current-point)
                  (find-steps pipe-map current-point))
        
        allowed-exit (first (remove walked exits))]
    (if (nil? allowed-exit) (conj walked current-point)
        (recur (conj walked current-point) pipe-map allowed-exit))))

(defn walk-loop
  [pipe-map start-point]
  (-walk-loop #{} pipe-map start-point))

(defn find-start
  ([pipe-map]
   (find-start 0 pipe-map))
  ([y pipe-map]
   (if (empty? pipe-map) nil
       (let [x (.indexOf (first pipe-map) (int \S))]
         (if (not= -1 x) [x y]
             (recur (inc y) (rest pipe-map)))))))
