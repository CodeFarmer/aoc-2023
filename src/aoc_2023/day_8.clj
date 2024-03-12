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

;; TODO refactor count-steps in terms of next-move
(defn next-move [graph d start-node]
  (let [[l r] (graph start-node)]
    (cond (= d \L) l
          (= d \R) r)))

(defn count-steps-until-all
  ([directions graph start-nodes pred]
   (count-steps-until-all 0 directions directions graph start-nodes pred))
  ([c directions original-directions graph start-nodes pred]
   (cond (every? pred start-nodes) c
         (empty? directions) (recur c original-directions original-directions graph start-nodes pred)
         :default (recur (inc c) (rest directions) original-directions graph (into [] (map #(next-move graph (first directions) %) start-nodes)) pred))))

;; exploring the problem space
(defn probe-cycles [c directions original-directions graph start-node pred]
  (cond (and (not (zero? c)) (pred start-node)) [c start-node directions]
        (empty? directions) (recur c original-directions original-directions graph start-node pred)
        :default (recur (inc c) (rest directions) original-directions graph (next-move graph (first directions) start-node) pred)))

(defn ends-with-z? [astr]
  (.endsWith astr "Z"))

(defn ends-with-a? [astr]
  (.endsWith astr "A"))

;; given two BigIntegers, find the lowest common multiple
(defn lcm [a b]
  (.multiply a (.divide b (.gcd a b))))
