(ns aoc-2023.day-13
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(defn map-rotate
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (map-rotate avec []))
  ([avec acc]
   (if (empty? (first avec))
     acc
     (recur (map rest avec) (conj acc (apply str (reverse (map first avec))))))))

(defn symmetrical-at?
  "Given a vector and an index, is the vector before that index repeated in reverse from there until the end of the vector?"
  [avec index]
  (let [left (take index avec)
        right (drop index avec)
        len (min (count left) (count right))]
    (= (take len (reverse left)) (take len right))))

(defn symmetry-line
  "Find the index of the item before which there is a line of symmetry"
  [avec]
  (some #(if (symmetrical-at? avec %) % nil) (range 1 (count avec))))

(defn vertical-symmetry-line [avec]
  (let [n (symmetry-line (map-rotate avec))]
    (if n n 0)))

(defn horizontal-symmetry-line [avec]
  (let [n (symmetry-line avec)]
    (if n n 0)))

(defn score [avec]
  (let [v (vertical-symmetry-line avec)
        h (horizontal-symmetry-line avec)
        s (+ v (* 100 h))]
    (comment 
      (println)
      (println (str/join "\n" avec))
      (println "v:" v)
      (println "h:" h)
      (println "s:" s))
    s))


