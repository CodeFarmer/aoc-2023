(ns aoc-2023.day-15
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(defn hhash
  ([astr]
   (hhash 0 astr))
  ([acc astr]
   (cond (empty? astr) acc
         (= \ (first astr)) (recur acc (rest astr))
         :default (recur (-> acc
                             (+ (int (first astr)))
                             (* 17)
                             (rem 256))
                         (rest astr)))))

(defn process-command [boxes astr]
  (let [[_ tag arg] (re-find (re-matcher #"(\w+)[=-](\d+)?" astr))
        h (hhash tag)
        box (boxes h)]
    
    (assoc boxes h (if arg (assoc box tag (Integer/parseInt arg))
                       (dissoc box tag)))))

(defn box-score [index amap]
  (let [ks (keys amap)]
    (->> (range 0 (count ks))
         (map #(* (inc index) (inc %) (get amap (nth ks %))))
         (reduce +))))
