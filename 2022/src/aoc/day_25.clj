(ns aoc.day-25
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 25 :name "example" :content "")
#_(aoc/day-input :day 25)

(defn parse [file]
  (slurp file))

(def example (parse "input/day25.example"))
(def input (parse "input/day25.input"))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day25")
 (fn [{:keys [result]}] result))
