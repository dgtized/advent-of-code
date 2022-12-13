(ns aoc.day-13
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

#_(aoc/day-input :day 13 :name "example" :content "")
#_(aoc/day-input :day 13)

{::clerk/visibility {:result :hide}}

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day13")
 (fn [{:keys [result]}] result))
