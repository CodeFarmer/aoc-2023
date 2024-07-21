(ns aoc-2023.core)

(defn lines-as-vector [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (line-seq rdr))))

(defn intify-seq
  "Transform a sequence of strings into a sequence of integers"
  [aseq]
  (map #(Integer/parseInt %) aseq))

(defn minverse [amap]
  (reduce (fn [a [k v]] (assoc a v k)) {} amap))

;; functions dealing with 2D maps expressed as vectors of
;; strings (rows)

(defn map-rotate
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (map-rotate avec []))
  ([avec acc]
   (if (empty? (first avec))
     acc
     (recur (map rest avec) (conj acc (apply str (reverse (map first avec))))))))

(defn get-tile
  "Given a map expressed as a vector of strings, find the tile character ar [x y]"
  [tile-map [x y]]
  (get-in tile-map [y x]))

