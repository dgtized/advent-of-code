(ns aoc.day-10
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 10 :name "example" :content "")
#_(aoc/day-input :day 10)

(defn parse [file]
  (slurp file))

(def example (parse "input/day10.example"))
(def input (parse "input/day10.input"))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day10")
 (fn [{:keys [result]}] result))
