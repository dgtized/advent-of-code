(ns aoc.day-03
  (:require [aoc.utility :refer [file->lines verify]]
            [nextjournal.clerk :as clerk]
            [clojure.set :as set]))

(defn score [c]
  (let [c (int c)]
    (inc (if (<= (int \A) c (int \Z))
           (+ 26 (- c (int \A)))
           (- c (int \a))))))

(comment (score \A)
         (score \a))

(defn split-sack [line]
  (let [n (count line)
        [c1 c2] (map set (split-at (/ n 2) line))]
    (score (first (set/intersection c1 c2)))))

^::clerk/no-cache
(reduce + (map split-sack (file->lines "input/day03.example")))
^::clerk/no-cache
(reduce + (map split-sack (file->lines "input/day03.input")))

(defn partition-sack [lines]
  (for [group (partition 3 lines)]
    (apply set/intersection (map set group))))

(reduce + (map (comp score first) (partition-sack (file->lines "input/day03.example"))))
(reduce + (map (comp score first) (partition-sack (file->lines "input/day03.input"))))
