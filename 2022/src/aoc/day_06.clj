(ns aoc.day-06
  ;; {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]))

(defn process [file size]
  (->> file
       file->lines
       first
       (partition size 1)
       (map-indexed vector)
       (some (fn [[i group]]
               (when (= (count (set group)) size)
                 (+ size i))))))

(process "input/day06.example" 4)
(process "input/day06.input" 4)
(process "input/day06.example" 14)
(process "input/day06.input" 14)

;; {::clerk/visibility {:result :show}} 
(answer-table
 [process]
 ["input/day06.example" "input/day06.input"]
 (fn [{:keys [result]}]
   result))
