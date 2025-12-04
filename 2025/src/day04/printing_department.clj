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

(defn remove-bales [grid bales]
  (reduce (fn [grid bale] (assoc grid bale \.)) grid bales))

(comment
  (let [grid (parse example)
        s1 (remove-bales grid (part1 grid))
        s2 (remove-bales s1 (part1 s1))]
    (println (ag/grid->str grid))
    (println)
    (println (ag/grid->str s1))
    (println)
    (println (ag/grid->str s2))
    nil))

(defn part2 [in]
  (loop [grid in removed 0]
    (let [bales (part1 grid)]
      (if (empty? bales)
        removed
        (recur
         (remove-bales grid bales)
         (+ removed (count bales)))))))

(assert (= 43 (part2 (parse example))))
(assert (= 8890 (part2 (parse input))))
