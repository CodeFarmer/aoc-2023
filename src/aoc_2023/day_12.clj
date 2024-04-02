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

(defn -check-arrangements [aseq rseq] (every? #(= aseq (-derive-arrangements [] 0 %)) rseq))

; (every? #(= len (count %)) %)

(defn arrangements [len aseq]
  (comment {:post [(-check-arrangements aseq %)
                   ]})
  (if (empty? aseq)
    [(apply str (repeat len \.))]
    (let [n (first aseq)
          block (apply str (repeat n \#))
          others (rest aseq)
          required-space (+ (count others) (reduce + others))]
      
      (reduce into []
              (for [pad-len (range 0 (- len required-space))
                    :let [padding (apply str (repeat pad-len \.))
                          prefix  (str padding block)
                          prefix-len (+ pad-len n)]]
                (cond (> prefix-len len) [] ;; one recursion endpoint, failure
                      (and (= prefix-len len) (empty? others)) [prefix] ;; other recursion endpoint, fill to the end
                      :default (map #(str prefix "." %) (arrangements (- len (inc prefix-len)) others))))))))

(defn *arrangements [len aseq]
  (let [r (arrangements len aseq)]
    (println "(arrangements " len aseq "): " r)
    r))

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

(defn *count-compatibles [astr avec]
  (let [ret (count-compatibles astr avec)]
    (println "count-compatibles:" astr avec ":" ret)
    ret))
