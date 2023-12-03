(ns aoc-2023.day-3
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def digits (into #{} "0123456789"))

;; FIXME this is absolutely horrific and I have regrets
;; Don't do this after a kids' birthday party and quite a bit of wine
;; If you are learning Clojure, look away now

(defn index-number-strings
  
  ([astr]
   (index-number-strings [] 0 [] astr))
  
  ([acc i digit-seq astr]
   (let [done? (= i (count astr))
         digit? (and (not done?) (digits (.charAt astr i)))
         digit-seq' (if digit? (conj digit-seq (.charAt astr i))
                        digit-seq)
         token-complete? (and (not (empty? digit-seq'))
                              (or done? (not digit?)))
         acc' (if token-complete?
                (conj acc [(- i (count digit-seq)) (apply str digit-seq)])
                acc)
         ]
     (if done? acc'
         (recur acc' (inc i) (if token-complete? [] digit-seq') astr)))))

(defn sigil? [c]
  (not (or (= \. c)
           (digits c))))

(defn sigil-at? [i astr]
  (cond (< i 0) false
        (>= i (count astr)) false
        :default (sigil? (.charAt astr i))))

(defn part-numbers

  ([lines]
   (let [empty-line (apply str (repeat (count (first lines)) \.))]
     (part-numbers [] (into [empty-line] (conj lines empty-line)))))

  ([acc lines]
   (if (empty? lines) acc
       (let [[prev current next & etc] lines
             indexed-nums (index-number-strings current)]

         (letfn [(part-number? [[i number-string]]
                   (let [min (dec i)
                         max (+ 1 i (count number-string))]

                     (or (some #(sigil-at? % prev) (range min max))
                         (sigil-at? min current)
                         (sigil-at? (+ i (count number-string)) current)
                         (some #(sigil-at? % next) (range min max)))))]
           

           (recur (into acc (map parse-long (map second (filter part-number? indexed-nums))))
                  (rest lines)))))))

