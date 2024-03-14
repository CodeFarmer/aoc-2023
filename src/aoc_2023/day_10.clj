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
  [walked pipe-map]
  (let [p (first walked)
        exits (if (= \S (get-tile pipe-map p)) (guess-exits pipe-map p)
                  (find-steps pipe-map (first walked)))
        
        allowed-exit (first (remove #(some #{%} walked) exits))]
    (if (nil? allowed-exit) walked
        (recur (conj walked allowed-exit) pipe-map))))

(defn walk-loop
  [pipe-map start-point]
  (-walk-loop (list start-point) pipe-map))

(defn find-start
  ([pipe-map]
   (find-start 0 pipe-map))
  ([y pipe-map]
   (if (empty? pipe-map) nil
       (let [x (.indexOf (first pipe-map) (int \S))]
         (if (not= -1 x) [x y]
             (recur (inc y) (rest pipe-map)))))))
