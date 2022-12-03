(ns aoc.day-03
  {:nextjournal.clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [file->lines verify]]
   [clojure.set :as set]
   [nextjournal.clerk :as clerk]))

(def sum (partial reduce +))

(defn score [c]
  (let [c (int c)]
    (inc (if (<= (int \A) c (int \Z))
           (+ 26 (- c (int \A)))
           (- c (int \a))))))

(defn split-sack [line]
  (let [n (count line)
        [c1 c2] (map set (split-at (/ n 2) line))]
    (score (first (set/intersection c1 c2)))))

(defn partition-sack [lines]
  (for [group (partition 3 lines)]
    (score (first (apply set/intersection (map set group))))))

{:nextjournal.clerk/visibility {:result :show}}
;; Verify score method
(verify (= 27 (score \A)))
(verify (= 1 (score \a)))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[part op] {"part1" (partial map split-sack)
                   "part2" partition-sack}
        file ["input/day03.example" "input/day03.input"]]
    (let [sacks (op (file->lines file))]
      [file part (sum sacks) sacks]))})
