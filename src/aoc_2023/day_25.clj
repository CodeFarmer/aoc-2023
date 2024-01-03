(ns aoc-2023.day-25
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]))


(defn parse-line [astr]
  (let [[node rest] (str/split astr #": ")
        edges (str/split rest #"\s")]
    [node (into #{} edges)]))

(defn merge-in [amap k aset]
  (assoc amap k (into (get amap k #{}) aset)))

(defn reverse-connection-sets [node conxs]
  (zipmap conxs
          (repeat #{node})))

(defn parse-graph
  ([lines]
   (parse-graph {} lines))
  ([acc lines]
   (if (empty? lines)
     acc
     (let [[node conxs] (parse-line (first lines))]
       (recur
        (merge-with #(into %1 %2)
                    (merge-in acc node conxs)
                    (reverse-connection-sets node conxs))
        (rest lines))))))

;; return a list of nodes traversed in order, and a set containing those nodes (yes this should use somebody's ordered set type
(defn flood-fill
  ([agraph start-node]
   (flood-fill [] #{} (conj clojure.lang.PersistentQueue/EMPTY start-node) agraph))
  ([acc seen q agraph]
   (if (empty? q) [acc seen]
       (let [n (peek q)]
         (recur (conj acc n)
                (conj seen n)
                (into (pop q) (filter #(not (seen %)) (agraph n)))
                agraph)))))

;; return maps containing the unconnected subgraphs of agraph
(defn sub-graphs
  ([agraph]
   (sub-graphs [] agraph))
  ([acc agraph]
   (if (empty? agraph) acc
       (let [start-node (first (keys agraph))
             [_ seen] (flood-fill agraph start-node)
             unseen-graph (apply (partial dissoc agraph) seen)
             seen-graph (select-keys agraph seen)]
         (recur (conj acc seen-graph) unseen-graph)))))

(defn disconnect [agraph node-1 node-2]
  (-> agraph
      (update node-1 #(disj % node-2))
      (update node-2 #(disj % node-1))))
