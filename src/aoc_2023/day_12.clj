(ns aoc-2023.day-12
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(defn arrangements [len aseq]
  (if (empty? aseq)
    [(apply str (repeat len \.))]
    (let [n (first aseq)
          block (apply str (repeat n \#))
          others (rest aseq)
          required-space (+ (count others) (reduce + others))]

      ;; map (range 0 ...)
      ;; this could be a smaller range but also doesn't matter
      (reduce into []
       (for [pad-len (range 0 (- len required-space))
             :let [padding (apply str (repeat pad-len \.))
                   prefix  (str padding block)
                   prefix-len (+ pad-len n)]]
         (cond (> prefix-len len) [] ;; one recursion endpoint, failure
               (= prefix-len len) [prefix] ;; other recursion endpoint, fill to the end
               :default (map #(str prefix "." %) (arrangements (- len (inc prefix-len)) others))))))))


(defn parse-line [astr]
  (let [[rec runstr] (str/split astr #" ")
        runs (intify-seq (str/split runstr #","))]
    [rec runs]))

(defn compatible? [pattern astr]
  (if (empty? pattern) (empty? astr)
      (let [p (first pattern)]
        (cond (= p \?) (compatible? (rest pattern) (rest astr))
              (= p (first astr)) (compatible? (rest pattern) (rest astr))
              :default false))))

(defn count-compatibles [astr avec]
  (count (filter (partial compatible? astr)
                 (arrangements (count astr) avec))))
