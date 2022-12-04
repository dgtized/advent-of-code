(ns aoc.day-04
  {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]))

(defn containing [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn overlapping [[a b c d]]
  (and (<= a d) (<= c b)))

(defn ->ranges [line]
  (map parse-long (re-seq #"\d+" line)))

(defn process [f]
  (fn [file]
    (filter (comp f ->ranges) (file->lines file))))

{::clerk/visibility {:result :show}}
(answer-table
 [(process containing) (process overlapping)]
 ["input/day04.example" "input/day04.input"]
 (fn [file star result]
   [file star (count result) result]))
