(ns aoc.day-03
  (:require
   [aoc.utility :refer [file->lines verify]]
   [clojure.set :as set]))

(defn score [c]
  (let [c (int c)]
    (inc (if (<= (int \A) c (int \Z))
           (+ 26 (- c (int \A)))
           (- c (int \a))))))

(verify (= 27 (score \A)))
(verify (= 1 (score \a)))

(defn split-sack [line]
  (let [n (count line)
        [c1 c2] (map set (split-at (/ n 2) line))]
    (score (first (set/intersection c1 c2)))))

(reduce + (map split-sack (file->lines "input/day03.example")))
(reduce + (map split-sack (file->lines "input/day03.input")))

(defn partition-sack [lines]
  (for [group (partition 3 lines)]
    (score (first (apply set/intersection (map set group))))))

(reduce + (partition-sack (file->lines "input/day03.example")))
(reduce + (partition-sack (file->lines "input/day03.input")))
