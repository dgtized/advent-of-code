(ns aoc.day-24
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 24 :name "example" :content "")
#_(aoc/day-input :day 24)

(defn parse [file]
  (slurp file))

(def example (parse "input/day24.example"))
(def input (parse "input/day24.input"))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day24")
 (fn [{:keys [result]}] result))
