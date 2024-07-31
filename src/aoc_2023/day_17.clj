(ns aoc-2023.day-17
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

;; approach: BFS (queue-based) where candidate squares are chosen if
;; they are a shorter route to the square than previously (kept in a
;; big map)
;;
;; also apply the three rule here
;;
;; once the end is reached stop adding candidates but follow up on all
;; previous candidates until there are none left

;; Wait I think I just restated Dijkstra's algorithm :facepalm:
;; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode

;; TODO move this into core

(defn find-lowest-path
  ([tmap]
   (find-lowest-path tmap [0 0] [dec (tmap-width tmap) (dec (tmap-height tmap))]))
  ([tmap start end]
   (find-lowest-path {start {:prev nil :cost 0}}
                     (conj clojure.lang.PersistentQueue/EMPTY start)
                     tmap start end))
  ([square-data q tmap p end]
   (if (empty? q)
     ;; unwind the traversal data back, TODO move this out?
     (loop [acc '()
            p end]
       (let [{:keys [cost prev]} (get square-data p)]
         (if (not prev) (conj acc p)
             (recur (conj acc p) prev))))
     
     (let [neighbours (tmap-find-neigbours tmap)]
       ;; TODO THIS IS WHERE YOU WERE
       ))))
