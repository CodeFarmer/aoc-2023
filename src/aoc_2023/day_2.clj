(ns aoc-2023.day-2
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn tok2pair [astr]
  (let [[i colour] (str/split astr #"\s")]
    [colour (Integer/parseInt i)]))

(defn draw-counts [astr] 
  (into {} (map tok2pair (str/split astr #", "))))

(defn possible? [known-bag astr]
  (letfn [(too-big [[k v]] (> v (get known-bag k)))]
    (not (some #(some too-big %)
               (map draw-counts (str/split astr #"; "))))))

(defn id-and-line [astr]
  (let [[id-str game-str] (str/split astr #": ")]
    [(Integer/parseInt (re-find #"\d+" id-str)) game-str]))

(defn possible-draws [known-bag lines]
  (letfn [(line-possible? [[id gamestr]] (possible? known-bag gamestr))]
    (map first (filter line-possible? (map id-and-line lines)))))

(defn minimum-counts [aseq]
  (reduce (partial merge-with max) {} aseq))

(defn minimum-counts-for-line [astr]
  (minimum-counts (map draw-counts (str/split astr #"; "))))

(defn power [amap]
  (reduce #(* %1 (second %2)) 1 amap))

(defn minimum-draws [lines]
  (map minimum-counts (map id-and-line lines)))
