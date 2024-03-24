(ns aoc-2023.core)

(defn lines-as-vector [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (into [] (line-seq rdr))))

(defn intify-seq
  "Transform a sequence of strings into a sequence of integers"
  [aseq]
  (map #(Integer/parseInt %) aseq))

(defn minverse [amap]
  (reduce (fn [a [k v]] (assoc a v k)) {} amap))
