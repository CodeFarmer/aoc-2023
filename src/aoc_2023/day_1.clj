(ns aoc-2023.day-1
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn first-and-last [astr]
  (intify-seq
   (let [rs (re-seq #"\d" astr)]
     [(first rs) (last rs)])))

(defn make-number [[a b]]
  (+ (* 10 a) b))

(defn -calibration-value [aseq fandl-f]
  (reduce +
          (map make-number
               (map fandl-f aseq))))

(defn calibration-value [aseq]
  (-calibration-value aseq first-and-last))

(def digits ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])

(defn wtoi [astr]
  (.indexOf digits astr))

(defn atoi [astr]
  (let [n (wtoi astr)]
    (if (= -1 n)
      (Integer/parseInt astr)
      n)))

(defn first-and-last-with-words [astr]
  (map atoi
       ;; this looks ugly because you have to use lookahead groups to deal with the "oneight" case
       (let [rs (map second
                     (re-seq (re-pattern (str "(?=("(str/join "|" digits) "|[\\d]))")) astr))]
     [(first rs) (last rs)])))

(defn calibration-value-with-words [aseq]
  (-calibration-value aseq first-and-last-with-words))
