(ns aoc-2023.day-5
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn parse-map [astr]
  (loop [acc {}
         ;; skip the first line which is the name
         lines (rest (str/split astr #"\n"))]
    (if (empty? lines)
      acc
      (let [line (first lines)
            numbers (map parse-long (str/split line #"\s+"))
            [k v1 v2] numbers]
        (recur (assoc acc k [v1 v2]) (rest lines))))))

(defn parse-almanac [chunk-strings]
  {:seeds (map parse-long (rest (str/split (first chunk-strings) #"\s+")))
   :maps (map parse-map (rest chunk-strings))})

(defn find-destination [amap source]
  (loop [ks (keys amap)]
    (if (empty? ks) source
        (let [k (first ks)
              [start len] (get amap k)]
          (comment (println source ": " k [start len]))
          (if (<= start source (+ start (dec len)))
            (+ k (- source start))
            (recur (rest ks)))))))

(defn walk-maps [loc maps]
  (if (empty? maps)
    loc
    (recur (find-destination (first maps) loc) (rest maps))))


;; partition a range into portions before, inside and after a partitioning range
(defn partition-range [[rstart rlen] [pstart plen]]
  (let [rend (+ rstart (dec rlen))
        pend (+ pstart (dec plen))]
    
    (comment (println rstart rend pstart pend))

    [(cond (>= rstart pstart) []
           (< rend pstart) [rstart rlen]
           :default [rstart
                     (- (min rend pstart) rstart)])
     
     (cond (> rstart pend) []
           (< rend pstart) []
           :default [(max rstart pstart) (- (inc (min rend pend))
                                            (max rstart pstart))])
     (cond (<= rend pend) []
           (> rstart pend) [rstart rlen]
           :default [(max (inc pend) rstart)
                     (- rend (max pend rstart))])])
  )

;; for the part of range inside prange, return it translated to
;; loc, followed by the ranges outside separately
(defn destinations-and-remainders [range loc prange]

  (let [[start _] prange
        [rs len] range
        [before inside after] (partition-range range prange)
        [istart ilen] inside]

    [(if (empty? inside)
       []
       [(+ istart (- loc start)) ilen])
     before
     after]))


(defn ranges-through-map-entry [acc spare k prange ranges]
  (if (empty? ranges)
    [acc spare]
    (let [[updated & others] (destinations-and-remainders (first ranges)
                                                          k
                                                          prange)]
      (recur (conj acc updated) (into spare others) k prange (rest ranges)))))

(defn ranges-through-map
  
  ([ranges amap]
   (ranges-through-map [] ranges amap))

  ([acc ranges amap]

   (loop [acc acc
          ks (keys amap)
          ranges ranges]

     (if (empty? ks)
       (distinct (remove empty? (into acc ranges)))
       (let [k (first ks)
             [good spares] (ranges-through-map-entry [] [] k (amap k) ranges)]
         (recur (into acc good)
                (rest ks)
                (remove empty? spares)))))))

(defn walk-maps-ranges [ranges maps]
  (if (empty? maps)
    ranges
    (recur (ranges-through-map ranges (first maps)) (rest maps))))
