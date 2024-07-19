(ns aoc-2023.day-12
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


;; for debugging
(defn -derive-arrangements [acc n astr]
  (if (empty? astr)
    (if (= 0 n)
      acc
      (conj acc n))
    (let [c (first astr)]
      (cond (= c \#) (recur acc (inc n) (rest astr))
            (> n 0)  (recur (conj acc n) 0 (rest astr))
            :default (recur acc 0 (rest astr))))))


(defn parse-line [astr]
  (let [[rec runstr] (str/split astr #" ")
        runs (intify-seq (str/split runstr #","))]
    [rec runs]))

(defn might-be-broken? [achar]
  (#{\# \?} achar))

(defn might-be-working? [achar]
  (#{\. \?} achar))

(defn run-possible-at?
  ([astr run-length start]
   (run-possible-at? astr run-length start false))
  ([astr run-length start pad?]
   (and
    (every? might-be-working? (take start astr))
    (every? might-be-broken? (take run-length (drop start astr)))
    (if pad? (and (> (count astr) (+ start run-length))
                  (might-be-working? (nth astr (+ start run-length))))
        true))))

(def count-compatibles
  (memoize 
   (fn [astr reports]
     (if
         (empty? reports) (if (every? might-be-working? astr)
                            1 
                            0)
         (let [r (first reports)
               rest (rest reports)
               starts (range 0 (- (count astr) (dec r)))
               pad? (not (empty? rest))]

           (->> starts
                (filter #(run-possible-at? astr r % pad?))
                (map (fn [run] (count-compatibles (.substring astr (+ run r (if pad? 1 0))) rest)))
                (reduce +))
           
           )
         ))))

(defn total-compatibles [data-set]
  (->> data-set
       (pmap (fn [[astr reports]] (count-compatibles astr reports)))
       (reduce +)))

(defn total-expanded-compatibles [data-set]
  (->> data-set
       (map (fn [[astr reports]] [(str/join "?" (repeat 5 astr))
                                  (flatten (repeat 5 reports))]))
       total-compatibles))
