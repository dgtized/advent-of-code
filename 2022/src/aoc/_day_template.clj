(ns aoc.day-10
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

#_(aoc/day-input :day 10 :name "example" :content "")
#_(aoc/day-input :day 10)

{::clerk/visibility {:result :hide}}

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day10")
 (fn [{:keys [result]}] result))
