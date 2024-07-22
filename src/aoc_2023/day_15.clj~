(ns aoc-2023.day-14
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.set]))

(def roll-left
  (memoize
   (fn [astr]
     (let [i     (.indexOf astr "O")
           block (.indexOf astr "#")]
       (if (or (= block -1) (< i block))
         
         (if (= -1 i) astr
             (if (= 0 i)
               (str "O" (roll-left (.substring astr 1)))
               (str "O"
                    (roll-left
                     (str (.substring astr 1 i) "." (.substring astr (inc i)))))))

         (str (.substring astr 0 (inc block)) (roll-left (.substring astr (inc block)))))))))

(defn score-rocks-distance-right
  "The score for a string is the total number of squares to the right (inclusive) of each O in it"
  [astr]
  (let [c (count astr)]
    (->> (range 0 c)
         (filter #(= \O (.charAt astr %)))
         (map #(- c %))
         (reduce +)))
  )

(defn total-score-rocks-distance-right [tmap]
  (reduce + (map score-rocks-distance-right tmap)))

;; rotating the board back again to get the score was annoying me
(defn total-score-rocks-distance-bottom [tmap]
  (let [height (count tmap)]
    (reduce +
            (map (fn [i] (* (- height i)
                            (count (filter #(= \O %) (nth tmap i)))))
                 (range 0 height)))))

;; Each cycle tilts the platform four times so that the rounded rocks
;; roll north, then west, then south, then east
(def spin-cycle
  (memoize 
   (fn [tmap]
     (->> tmap
          (tmap-rotate-left)
          (pmap roll-left)
          (tmap-rotate-right)
          (pmap roll-left)
          (tmap-rotate-right)
          (pmap roll-left)
          (tmap-rotate-right)
          (pmap roll-left)
          ;; FIXME should be able to flip in a single action
          (tmap-rotate-right)
          (tmap-rotate-right)))))
