(ns aoc-2023.day-6
  (:require [aoc-2023.core :refer :all]
            [clojure.string :as str]
            [clojure.math :as m]))

; y = -x^2 + rx
(defn distance-travelled [hold race-duration]
  (* hold (- race-duration hold)))

;; lowest and highest hold times that will win a race
(defn winning-range [race-duration best-time]
  (let [delta (m/sqrt (- (* race-duration race-duration) (* 4 best-time)))]
    ;; make sure you are strictly greater/less than numbers that hit
    ;; the record exactly
    [(int (m/floor  (inc (/ (- race-duration delta) 2))))
     (int (m/ceil   (dec (/ (+ race-duration delta) 2))))]))
