(ns aoc.day-04
  (:require
   [aoc.utility :refer [file->lines]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

(defn containing [[a b c d]]
  (or (and (<= a c) (<= d b))
      (and (<= c a) (<= b d))))

(defn overlapping [[a b c d]]
  (or (<= c a d)
      (<= c b d)
      (<= a c b)
      (<= a d b)))

(defn parse-line [line]
  (mapcat (fn [part] (map parse-long (str/split part #"-")))
          (str/split line #",")))

(defn parse [f input]
  (filter f (map parse-line input)))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[star method] {1 (partial parse containing)
                       2 (partial parse overlapping)}
        file ["input/day04.example" "input/day04.input"]]
    (let [result (method (file->lines file))]
      [file star (count result) result]))})
