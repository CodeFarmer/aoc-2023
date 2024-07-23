(ns aoc-2023.day-16
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def reflections
  { \\ {:right [:down]
        :left  [:up]
        :down  [:right]
        :up    [:left]}
    \/ {:right [:up]
        :left  [:down]
        :down  [:left]
        :up    [:right]}
    \- {:right [:right]
        :left  [:left]
        :down  [:left :right]
        :up    [:left :right]}
    \| {:right [:up :down]
        :left  [:up :down]
        :down  [:down]
        :up    [:up]}
   })

(defn next-dirs [dir tile]
  (if (= \. tile) [dir]
      (get-in reflections [tile dir])))

(defn next-squares [dir point tmap]
  (let [width (count (first tmap))
        height (count tmap)]
    (filter (fn [[_ [x y]]] (and (>= x 0) (>= y 0) (< x width) (< y height)))
            (->> (next-dirs dir (get-tile tmap point))
                 (map (fn [dir]
                        (let [delta (get directions dir)]
                          [dir (map + point delta)])))
                 )
            )))


(defn -energized-tiles
  [seen-steps next-steps tmap]
  (if (empty? next-steps)
    (into #{} (map (fn [[_ point]] point) seen-steps))
    (let [[dir point] (first next-steps)
          further-steps (next-squares dir point tmap)]
      (recur (conj seen-steps [dir point])
             ;; don't forget to avoid cycles
             (concat (filter #(not (seen-steps %)) further-steps) (rest next-steps))
             tmap))))

(defn energized-tiles
  ([dir point tmap]
   (-energized-tiles #{[dir point]} (next-squares dir point tmap) tmap)))

(defn starting-points [tmap]
  (let [h (count tmap)
        w (count (first tmap))
        hi (range 0 h)
        wi (range 0 w)]
    (concat
     (map (fn [x] [:down  [x 0]]) wi)
     (map (fn [y] [:right [0 y]]) hi)
     (map (fn [x] [:up    [x (dec h)]]) wi)
     (map (fn [y] [:left  [(dec w) y]]) hi))))
