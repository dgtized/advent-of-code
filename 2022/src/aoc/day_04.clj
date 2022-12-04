(ns aoc.day-04
  (:require
   [aoc.utility :refer [file->lines verify]]
   [nextjournal.clerk :as clerk]))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[star method] {1 identity
                       2 identity}
        file ["input/day04.example" "input/day04.input"]]
    (let [result (method (file->lines file))]
      [file star result]))})
