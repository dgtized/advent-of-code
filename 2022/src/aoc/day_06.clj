(ns aoc.day-06
  (:require
   [aoc.utility :refer [answer-table]]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}
(defn process [size file]
  (->> file
       slurp
       (partition size 1)
       (map-indexed vector)
       (some (fn [[i group]]
               (when (= (count (set group)) size)
                 (+ size i))))))

{::clerk/visibility {:result :show}}
(answer-table
 [(partial process 4) (partial process 14)]
 ["input/day06.example" "input/day06.input"]
 (fn [{:keys [result]}] result))
