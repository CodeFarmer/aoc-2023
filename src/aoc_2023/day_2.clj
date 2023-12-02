(ns aoc-2023.day-2
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn tok2pair [astr]
  (let [[i colour] (str/split astr #"\s")]
    [colour (Integer/parseInt i)]))

(defn draw-counts [astr] 
  (into {} (map tok2pair (str/split astr #", "))))

(defn possible? [known-bag astr]
  (let [draws (map draw-counts (str/split astr #"; "))]
    (letfn [(too-big [[k v]] (> v (get known-bag k)))]
      (not (some #(some too-big %) draws)))))

(defn id-and-line [astr]
  (let [[id-str game-str] (str/split astr #": ")]
    [(Integer/parseInt (re-find #"\d+" id-str)) game-str]))

(defn possible-draws [known-bag lines]
  (letfn [(line-possible? [[id gamestr]] (possible? known-bag gamestr))]
    (map first (filter line-possible? (map id-and-line lines)))))

(defn minimum-counts
  ([aseq] (minimum-counts {} aseq))
  ([acc aseq]
   (if (empty? aseq)
     acc
     (recur 
      (merge-with max acc (first aseq))
      (rest aseq)))))

(defn minimum-counts-for-line [astr]
  (let [draws (map draw-counts (str/split astr #"; "))]
    (minimum-counts draws)))

(defn power [amap]
  (reduce #(* %1 (second %2)) 1 amap))

(defn minimum-draws [lines]
  (map minimum-counts (map id-and-line lines)))
