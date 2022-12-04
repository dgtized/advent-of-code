(ns aoc.day-04
  (:require
   [aoc.utility :refer [answer-table file->lines]]))

(defn containing [[a b c d]]
  (or (<= a c d b) (<= c a b d)))

(defn overlapping [[a b c d]]
  (and (<= a d) (<= c b)))

(defn ->ranges [line]
  (map parse-long (re-seq #"\d+" line)))

(defn parse [f]
  (filter (comp f ->ranges)))

(answer-table
 [(parse containing) (parse overlapping)]
 ["input/day04.example" "input/day04.input"]
 (fn [file star method]
   (let [result (into [] method (file->lines file))]
     [file star (count result) result])))
