(ns aoc-2023.day-25-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-25 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(def example-data (str/split
                   "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"
                   #"\n"))

(deftest parse-line-test
  (testing "Parsing of an input line into a node and a list of connections"
    (is (= ["jqt" #{"rhn" "xhk" "nvd"}]
           (parse-line (first example-data))))))

(deftest merge-in-test
  (is (= {:a #{:b}} (merge-in {} :a #{:b})))
  (is (= {:a #{:b :c}} (merge-in {:a #{:b}} :a #{:c}))))

(deftest reverse-connection-sets-test
  (is (= {:b #{:a}
          :c #{:a}}
         (reverse-connection-sets :a [:b :c]))))

(deftest parse-graph-test
  (testing "Parsing of input into nodes and edges"
    (let [nodes (parse-graph example-data)]
      (is (= 15 (count nodes))))))

(deftest flood-fill-test
  (let [[path seen] (flood-fill {:a #{:b :c}
                                 :b #{:a}
                                 :c #{:a :d}
                                 :d #{:c}}
                                :a)]
    (is (= 4 (count path)))
    (is (= :a (first path)))
    (is (= :d (last path)))))

(deftest node-disconnection-test
  (let [tree {:a #{:b :c}
              :b #{:a}
              :c #{:a :d}
              :d #{:c}}]
    (is (= {:a #{:b}
              :b #{:a}
              :c #{:d}
              :d #{:c}}
           (disconnect tree :a :c)))))

(deftest count-disconnected-graphs-test
  (let [agraph (parse-graph example-data)
        bgraph (-> agraph
                   (disconnect "hfx" "pzl")
                   (disconnect "bvb" "cmg")
                   (disconnect "nvd" "jqt"))]
    (is (= 1 (count (sub-graphs agraph))))
    (let [sub-graphs (sub-graphs bgraph)]
      (is (= 2 (count sub-graphs)))
      (is (= 54 (* (count (keys (first sub-graphs)))
                   (count (keys (second sub-graphs)))))))))
