(ns day08.movie-theater
  (:require [clojure.string :as str]
            [aoc.combinatorics :as ac]))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (mapv parse-long (re-seq #"\d+" line))))

(defn area [[[x0 y0] [x1 y1]]]
  (* (inc (abs (- x1 x0))) (inc (abs (- y1 y0)))))

(comment (area [[2 5] [9 7]])
         (area [[9 7] [2 5]]))

(defn part1 [in]
  (mapv area (ac/all-pairs in)))

(assert (= 50 (apply max (part1 (parse example)))))
(assert (= 4781546175 (apply max (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
