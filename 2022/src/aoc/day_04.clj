(ns aoc.day-04
  (:require
   [aoc.utility :refer [file->lines]]
   [nextjournal.clerk :as clerk]))

(defn containing [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn overlapping [[a b c d]]
  (and (<= a d) (<= c b)))

(defn ->ranges [line]
  (map parse-long (re-seq #"\d+" line)))

(defn parse [f input]
  (filter (comp f ->ranges) input))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[star method] {1 (partial parse containing)
                       2 (partial parse overlapping)}
        file ["input/day04.example" "input/day04.input"]]
    (let [result (method (file->lines file))]
      [file star (count result) result]))})
