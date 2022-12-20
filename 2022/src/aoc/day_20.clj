(ns aoc.day-20
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 20 :name "example" :content "")
#_(aoc/day-input :day 20)

(defn parse [file]
  (map parse-long (aoc/file->lines file)))

(parse "input/day20.example")
(parse "input/day20.input")

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day20")
 (fn [{:keys [result]}] result))
