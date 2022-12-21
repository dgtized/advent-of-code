(ns aoc.day-21
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 21 :name "example" :content "")
#_(aoc/day-input :day 21)

(defn parse [file]
  file)

(def example (parse "input/day21.example"))
(def input (parse "input/day21.input"))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day21")
 (fn [{:keys [result]}] result))
