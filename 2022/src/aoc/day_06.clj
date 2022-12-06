(ns aoc.day-06
  ;; {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]))

(defn process [file]
  (->> file
       file->lines
       first
       (partition 4 1)
       (map-indexed vector)
       (some (fn [[i group]]
               (when (= (count (set group)) 4)
                 (+ 4 i))))))

(process "input/day06.example")
(process "input/day06.input")

;; {::clerk/visibility {:result :show}} 
(answer-table
 [process]
 ["input/day06.example" "input/day06.input"]
 (fn [{:keys [result]}]
   result))
