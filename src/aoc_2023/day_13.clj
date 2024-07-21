(ns aoc-2023.day-13
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.set]))


(defn symmetrical-at?
  "Given a vector and an index, is the vector before that index repeated in reverse from there until the end of the vector?"
  [avec index]
  (let [left (take index avec)
        right (drop index avec)
        len (min (count left) (count right))]
    (= (take len (reverse left)) (take len right))))

(defn all-symmetry-lines [avec]
  (filter #(symmetrical-at? avec %) (range 1 (count avec))))

(defn symmetry-line
  "Find the index of the item before which there is a line of symmetry"
  [avec]
  (first (all-symmetry-lines avec)))

(defn all-vertical-symmetry-lines [avec]
  (all-symmetry-lines (map-rotate avec)))

(defn vertical-symmetry-line [avec]
  (let [n (first (all-vertical-symmetry-lines avec))]
    (if n n 0)))

(def all-horizontal-symmetry-lines all-symmetry-lines)
(defn horizontal-symmetry-line [avec]
  (let [n (symmetry-line avec)]
    (if n n 0)))

(defn score [avec]
  (let [v (vertical-symmetry-line avec)
        h (horizontal-symmetry-line avec)
        s (+ v (* 100 h))]
    s))


(defn -smudge [achar]
  (if (= \# achar) \.
      \#))

(defn smudge-at [avec x y]
  (let [row (nth avec y)
          c (.charAt row x)]
      (assoc avec y
             (str (.substring row 0 x) (-smudge c) (.substring row (inc x))))))

(defn generate-smudges [avec]
  (for [x (range 0 (count (first avec)))
        y (range 0 (count avec))]
    (smudge-at avec x y)))


(defn different-items [lista listb]
  (clojure.set/difference (into #{} lista)
                          (into #{} listb)))

(defn find-smudged-reflection
  "Return a vector containing [vertical-line horizontal-line] of the new line of symmetry formed by smudging one of the squares in the map"
  [avec]
  (loop [h0 (horizontal-symmetry-line avec)
         v0 (vertical-symmetry-line avec)
         smudged (generate-smudges avec)]
    (let [svec (first smudged)
          r (rest smudged)
          h (into #{} (all-horizontal-symmetry-lines svec))
          v (into #{} (all-vertical-symmetry-lines svec))
          h' (disj h h0)
          v' (disj v v0)]
      (cond (not (empty? h')) [0 (first h')]
            (not (empty? v')) [(first v') 0]
            (empty? r) []
            :default (recur h0 v0 r)))))


(defn smudged-score [avec]
  (let [[v1 h1] (find-smudged-reflection avec)]
    (+ (* 100 h1)
       v1)))
