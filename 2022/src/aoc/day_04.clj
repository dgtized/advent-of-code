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

(defn parse [f]
  (filter (comp f ->ranges)))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[star method] {1 (parse containing)
                       2 (parse overlapping)}
        file ["input/day04.example" "input/day04.input"]]
    (let [result (into [] method (file->lines file))]
      [file star (count result) result]))})
