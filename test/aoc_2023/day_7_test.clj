(ns aoc-2023.day-7-test
  (:require [clojure.test :refer :all]
            [aoc-2023.day-7 :refer :all]
            [aoc-2023.core :refer :all]
            [clojure.string :as str]))

(deftest five-of-a-kind-test
  (is (not (five-of-a-kind? "BAAAA")))
  (is (not (five-of-a-kind? "AAABA")))
  (is (five-of-a-kind? "AAAAA")))

(deftest counts-test
  (is (= {\A 5} (card-counts "AAAAA")))
  (is (= {\A 4 \B 1} (card-counts "BAAAA")))
  (is (= {\A 2 \B 1 \C 2} (card-counts "BACCA"))))

(deftest four-of-a-kind-test
  (is (not (four-of-a-kind? "AABBB")))
  (is (not (four-of-a-kind? "BBBBB")))
  (is (four-of-a-kind? "BBABB")))

(deftest full-house-test
  (is (not (full-house? "ABCCC")))
  (is (not (full-house? "ABCCC")))
  (is (not (full-house? "AAAAA")))
  (is (not (full-house? "BAAAA")))
  (is (full-house? "BBAAA"))
  (is (full-house? "ACACA")))

(deftest three-of-a-kind-test
  (is (not (three-of-a-kind? "ABCDE")))
  (is (not (three-of-a-kind? "AACDE")))
  (is (not (three-of-a-kind? "AABBB")))                       
  (is (three-of-a-kind? "AAADE")))

(deftest two-pair-test
  (is (not (two-pair? "AABCD")))
  (is (not (two-pair? "AABBB")))
  (is (not (two-pair? "ABCDE")))
  (is (two-pair? "AABBC"))
  (is (two-pair? "ABCBA")))

(deftest one-pair-test
  (is (not (one-pair? "ABCDE")))
  (is (one-pair? "ABBCD"))
  (is (not (one-pair? "AABBC")))
  (is (not (one-pair? "AABBB"))))

(deftest stronger-test
  (is (stronger? "AAAAA" "BAAAA"))
  (is (not (stronger? "BAAAA" "AAAAA")))
  (is (stronger? "BAAAA" "BBAAA"))
  (is (not (stronger?  "BBAAA" "BAAAA")))
  (is (stronger? "BBAAA" "BBAAC"))
  (is (not (stronger? "BBAAC" "BBAAA")))
  (is (stronger? "BBAAC" "BBACD"))
  (is (not (stronger? "BBACD" "BBAAC")))
  (is (stronger? "BBACD" "ABCDE"))
  (is (not (stronger? "ABCDE" "BBACD")))
  ;; equal hand strength tie breakers
  (is (stronger? "33332" "2AAAA"))
  (is (not (stronger? "2AAAA" "33332")))
  (is (stronger? "77888" "77788"))
  (is (not (stronger? "77788" "77888")))
  (is (stronger? "AT345" "TA345"))
  (is (not (stronger? "TA345" "AT345"))))

(defn sample-splitter [[a b]]
  [a (parse-long b)])

(def sample-data
  (map sample-splitter
       (partition 2
                  (str/split "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"
                             #"\s+"))))

(deftest hand-rank-test
  (testing "ranks calculated correctly for hands"
    (let [sample-hands (map first sample-data)
          hand-ranks (generate-hand-ranks sample-hands)]
      (is (= [1 4 3 2 5]
             (map #(hand-rank hand-ranks %) sample-hands))))))

(deftest winnings-test
  (testing "winnings are calculated correctly"
    (let [sample-hands (map first sample-data)
          hand-ranks (generate-hand-ranks sample-hands)]
      (is [= 6440
           (reduce +
                   (map #(apply (partial winnings hand-ranks) %)
                        sample-data))]))))

(def input-data
  (map sample-splitter
       (partition 2
                  (str/split (slurp "aoc-2023-inputs/input-7.txt")
                             #"\s+"))))

(deftest part-1-test
  (let [hands (map first input-data)
        hand-ranks (generate-hand-ranks hands)]
    (is [= 250453939
         (reduce +
                 (map #(apply (partial winnings hand-ranks) %)
                      input-data))])))

;; part 2

(deftest joker-substitution-test
  (is (= "T5555" (best-j-substitution "T55J5")))
  (is (= "KTTTT" (best-j-substitution "KTJJT")))
  (is (= "QQQQA" (best-j-substitution "QQQJA"))))

(deftest winnings-with-jokers-test
  (let [hands (map first sample-data)
        hand-ranks (generate-hand-ranks hands stronger-jokers?)]
    (is [= 5905
         (reduce +
                 (map #(apply (partial winnings hand-ranks) %)
                      sample-data))])))


(deftest part-2-test
  (testing "winnings are calculated correctly with Joker substitution for the whole input"
    (let [hands (map first input-data)
        hand-ranks (generate-hand-ranks hands stronger-jokers?)]
    (is [= 248652697
         (reduce +
                 (map #(apply (partial winnings hand-ranks) %)
                      input-data))]))))
