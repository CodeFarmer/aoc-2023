(ns aoc-2023.day-7
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn five-of-a-kind? [astr]
  (= 1 (count (into #{} astr))))

(defn card-counts
  ([astr]
   (card-counts {} astr))
  ([acc astr]
   (if (empty? astr)
     acc
     (let [c (first astr)]
       (recur (assoc acc c (inc (get acc c 0))) (rest astr))))))

(defn four-of-a-kind? [astr]
  (= #{1 4} (into #{} (vals (card-counts astr)))))

(defn full-house? [astr]
  (= #{2 3} (into #{} (vals (card-counts astr)))))

(defn full-house? [astr]
  (= #{2 3} (into #{} (vals (card-counts astr)))))

(defn three-of-a-kind? [astr]
  (= #{3 1} (into #{} (vals (card-counts astr)))))

(defn two-pair? [astr]
  (= 2 (count (filter #(= 2 %) (vals (card-counts astr))))))

(defn one-pair? [astr]
  (let [counts (vals (card-counts astr))]
    (and (= 1 (count (filter #(= 2 %) counts)))
         (not (some #(= 3 %) counts)))))

(def strength-ranking
  [five-of-a-kind?
   four-of-a-kind?
   full-house?
   three-of-a-kind?
   two-pair?
   one-pair?])

(defn -strength-ordinal
  ([astr]
   (-strength-ordinal strength-ranking 0 astr))
  ([ranks ord astr]
   (cond (empty? ranks) ord
         ((first ranks) astr) ord
         :default (recur (rest ranks) (inc ord) astr))))

(def card-ranking "AKQJT987654321")
(defn -card-ordinal [achar]
  ;; bah to Java's indexOf requiring String arg
  (.indexOf card-ranking (str achar)))

(defn -tie-breaker [astr bstr]
  (let [aord (-card-ordinal (first astr))
        bord (-card-ordinal (first bstr))]
    (cond (empty? astr) nil
          (not (= aord bord)) (< aord bord)
          :default (recur (rest astr) (rest bstr)))))

(defn stronger? [astr bstr]
  (let [aord (-strength-ordinal astr)
        bord (-strength-ordinal bstr)]
    (if (= aord bord)
      (-tie-breaker astr bstr)
      (< aord bord))))

(defn generate-hand-ranks [hands]
  (into [] (reverse (sort stronger? hands))))

(defn hand-rank [hand-ranks hand]
  ;; this can certainly be made faster just with the sort
  (inc (.indexOf hand-ranks hand)))

(defn winnings [hand-ranks hand bid]
  (*  bid (hand-rank hand-ranks hand)))
