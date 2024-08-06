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

(defn unwind-path [square-data end]
  (loop [acc '()
         p end]
    (let [{:keys [cost prev]} (get square-data p)]
      (if (not prev) (conj acc p)
          (recur (conj acc p) prev)))))

;; consider making the map contain ints and not do the ctoi
(defn cost-from [square-data tmap p q]
  "how much is the journey cost to q, when coming from p with a known journey cost"
  (+ (ctoi (get-tile tmap q)) (:cost (square-data p))))

(defn find-lower-paths [square-data tmap p points]
  (reduce (fn [new-data q]
             (let [d  (get-in square-data [q :cost] Integer/MAX_VALUE)
                   d' (cost-from square-data tmap p q)]
               (if (< d' d) (assoc new-data q {:prev p :cost d'})
                   new-data)))
          {}
          points))

(defn find-lowest-path
  ([tmap]
   (find-lowest-path tmap [0 0] [(dec (tmap-width tmap)) (dec (tmap-height tmap))]))
  ([tmap start end]
   (find-lowest-path {start {:prev nil :cost 0}}
                     (conj clojure.lang.PersistentQueue/EMPTY start)
                     tmap
                     end))

  ([square-data ;; annotations for visited squares
    q           ;; current squares under consideration [square prev]
    tmap 
    end]
   
   (if (empty? q)
     (unwind-path square-data end)
     
     (let [p (peek q)]
       (if (= end p)
         (recur square-data (pop q) tmap end)
         (let [neighbours (tmap-find-neighbours p tmap)
               lower-neighbour-data (find-lower-paths square-data tmap p neighbours)]
           (recur (into square-data lower-neighbour-data)
                  (into (pop q) (keys lower-neighbour-data))
                  tmap
                  end)))

       ))))
