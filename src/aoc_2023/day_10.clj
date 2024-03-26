(ns aoc-2023.day-10
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

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

(defn neighbours-entering [pipe-map [x y]]
  (into #{} (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                  :let [x' (+ x dx) y' (+ y dy)]
                  :when (contains? (find-steps pipe-map [x' y']) [x y])]
              [dx dy])))

(defn guess-exits [pipe-map [x y]]

  (into #{}
        (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
              :let [x' (+ x dx) y' (+ y dy)]
              :when (contains? (find-steps pipe-map [x' y']) [x y])]
          [x' y'])))

(defn find-start
  ([pipe-map]
   (find-start 0 pipe-map))
  ([y pipe-map]
   (if (empty? pipe-map) nil
       (let [x (.indexOf (first pipe-map) (int \S))]
         (if (not= -1 x) [x y]
             (recur (inc y) (rest pipe-map)))))))

(defn -walk-loop
  [walked pipe-map current-point]
  (let [exits (if (= \S (get-tile pipe-map current-point)) (guess-exits pipe-map current-point)
                  (find-steps pipe-map current-point))
        
        allowed-exit (first (remove walked exits))]
    (if (nil? allowed-exit) (conj walked current-point)
        (recur (conj walked current-point) pipe-map allowed-exit))))

(defn walk-loop
  ([pipe-map]
   (walk-loop pipe-map (find-start pipe-map)))
  ([pipe-map start-point]
   (-walk-loop #{} pipe-map start-point)))

;; part 2

(def exits-to-tiles
  (minverse exit-offsets))

(defn guess-tile [pipe-map [x y]]
  (exits-to-tiles (neighbours-entering pipe-map [x y])))

(defn guess-start-tile [pipe-map]
  (guess-tile pipe-map (find-start pipe-map)))

;; reading a line from left to right, starting from the outside, only
;; certain certain state transitions are possible through encountering
;; loop tiles
(def scan-states
  {:outside {\F :top
             \| :inside
             \L :bottom}
   :inside  {\F :bottom
             \| :outside
             \L :top}
   :top     {\7 :outside
             \J :inside
             \- :top}
   :bottom  {\7 :inside
             \J :outside
             \- :bottom}})


(defn count-row-enclosed-tiles
  ([astr y aloop]
   (count-row-enclosed-tiles astr 0 0 y aloop :outside))
  ([astr n x y aloop state]
   (cond (>= x (count astr)) n
         (aloop [x y]) (recur astr n (inc x) y aloop (get-in scan-states [state (.charAt astr x)]))
         (= state :inside) (recur astr (inc n) (inc x) y aloop state)
         :default (recur astr n (inc x) y aloop state))))

(defn correct-start-tile [pipe-map]
  (let [[x y] (find-start pipe-map)
        t (guess-tile pipe-map [x y])]
    (assoc pipe-map y (str (doto (StringBuilder. (pipe-map y))
                                      (.setCharAt x t))))))

(defn count-enclosed-tiles
  ([pipe-map aloop]
   (count-enclosed-tiles 0 0 pipe-map aloop))
  ([n y pipe-map aloop]
   (if (empty? pipe-map) n
       (recur (+ n (count-row-enclosed-tiles (first pipe-map) y aloop))
              (inc y)
              (rest pipe-map)
              aloop))))

;; end solution ;; end solution ;; end solution ;; end solution
;; and now, the stuff that didn't work

;; assume the map is rectangular eh
(defn find-edges [pipe-map]
  (let [w (count (first pipe-map))
        h (count pipe-map)]
    (-> #{}
        (into (map (fn [x] [x 0]) (range 0 w)))
        (into (map (fn [y] [0 y]) (range 1 (dec h))))
        (into (map (fn [y] [(dec w) y]) (range 1 (dec h))))
        (into (map (fn [x] [x (dec h)]) (range 0 w))))))

(defn find-flood-seeds [pipe-map loop]
  (set/difference (find-edges pipe-map) loop))

(defn flood-fill
  ([pipe-map loop-squares]
   (let [seeds (find-flood-seeds pipe-map loop-squares)]
     (flood-fill (into #{} seeds)
                 (into clojure.lang.PersistentQueue/EMPTY seeds)
                 (count (first pipe-map))
                 (count pipe-map)
                 loop-squares)))
  ([acc q width height loop-squares]
   (if (empty? q)
     acc
     (let [[x y] (peek q)
           q' (pop q)
           ;; TODO maybe DRY up the neighbour-finding
           neighbours (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                            :let [x' (+ x dx) y' (+ y dy)]
                            :when (and (>= x' 0) (< x' width)
                                       (>= y' 0) (< y' height)
                                       (not (acc [x' y']))
                                       (not (loop-squares [x' y'])))]
                        [x' y'])]
       (recur (into acc neighbours) (into q' neighbours) width height loop-squares)))))

(defn show-square-str [pipe-map-vec square-set]
  (if (empty? square-set)
    (str/join "\n" pipe-map-vec)
    (let [[x y] (first square-set)]
      (recur (assoc pipe-map-vec y (str
                                    (doto (StringBuilder. (pipe-map-vec y))
                                      (.setCharAt x \O))))
             (disj square-set [x y])))))

