(ns day04.printing-department
  (:require [aoc.grid :as ag]
            [aoc.vector :as v]
            [clojure.string :as str]))

(def input (slurp "src/day04/input"))
(def example (slurp "src/day04/example"))

(defn parse [in]
  (ag/lines->grid (str/split-lines in)))

(defn neighbors [g loc]
  (for [dir v/directions
        :let [pos (v/v+ loc dir)]
        :when (= (get g pos nil) \@)]
    pos))

(comment (neighbors (parse example) [1 1]))

(defn part1 [grid]
  (filter (fn [pos] (< (count (neighbors grid pos)) 4))
          (ag/keep-values grid \@)))

(assert (= 13 (count (part1 (parse example)))))
(assert (= 1489 (count (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
