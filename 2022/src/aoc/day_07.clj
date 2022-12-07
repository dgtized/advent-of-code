(ns aoc.day-07
  (:require
   [aoc.utility :refer [answer-table]]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}
(defn process [file]
  (slurp file))

{::clerk/visibility {:result :show}}
(answer-table
 [(partial process) (partial process)]
 ["input/day07.example" "input/day07.input"]
 (fn [{:keys [result]}] result))
