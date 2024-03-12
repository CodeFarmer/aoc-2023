(ns aoc-2023.day-8
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(defn parse-node-line [astr]
  (re-seq #"\w+" astr))

(defn parse-data-string [astr]
  (let [lines (str/split astr #"\n")
        [directions _ & map-lines] lines]
    [directions (reduce (fn [amap [name l r]] (assoc amap name [l r]))
                        {}
                        (map parse-node-line map-lines))]))

(defn count-steps
  ([directions graph start-node end-node]
   (count-steps 0 directions directions graph start-node end-node))
  ([c directions original-directions graph start-node end-node]
   (cond (= start-node end-node) c
         (empty? directions) (recur c original-directions original-directions graph start-node end-node)
         :default (let [d (first directions)
                        [l r] (graph start-node)
                        dest (cond (= d \L) l
                                   (= d \R) r
                                   :default (throw (RuntimeException. (str "Unexpected direction '" d "'"))))]
                    (recur (inc c) (rest directions) original-directions graph dest end-node)))))

