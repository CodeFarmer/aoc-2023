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

;; functions dealing with 2D maps expressed as vectors of
;; strings (rows)

(defn tmap-rotate-right
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (tmap-rotate-right avec []))
  ([avec acc]
   (if (empty? (first avec))
     acc
     (recur (map rest avec) (conj acc (apply str (reverse (map first avec))))))))

(defn tmap-rotate-left
  "Given a map expressed as a vector of strings (each a single line of the map), rotate it 90 degrees clockwise"
  ([avec] (tmap-rotate-left avec '()))
  ([avec acc]
   (if (empty? (first avec))
     (into [] acc)
     (recur (map rest avec) (conj acc (apply str (map first avec)))))))

(defn get-tile
  "Given a map expressed as a vector of strings, find the tile character ar [x y]"
  [tile-map [x y]]
  (get-in tile-map [y x]))

;; finding cycles in sequences

;; assumptions: the sequence is deterministic in that once the first
;; member of a cycle appears, the rest of the cycle is certain to
;; follow it
;; (for example, the output of core.iterate)

(defn find-cycle
  "In a deterministic stateless sequence (for example the output of core.iterate), find the first index of the beginning of a cycle, and return the cycle contents"
  ([aseq]
   (find-cycle #{} aseq aseq))
  ([acc aseq looking-seq]
   (if (empty? looking-seq) nil
       (let [x (first looking-seq)]
         (if (acc x) ;; just finished the cycle  
           (let [start (drop-while #(not (= x %)) aseq)
                 tail (take-while #(not (= x %)) (rest start))]
             (conj tail x))
           (recur (conj acc x) aseq (rest looking-seq)))
         ))))

(defn nth-with-cycles
  "Given a sequence with a cycle in it, guess the nth element of the sequence"
  [aseq n]
  (let [cyc (find-cycle aseq)
        cl (count cyc)
        x (first cyc)
        prelude (take-while #(not (= x %)) aseq)
        pl (count prelude)]
    (if (< n pl)
      (nth prelude n)
      (nth cyc (mod (- n pl) cl)))))
