(ns aoc.day-06
  ;; {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]))

(defn process [size file]
  (->> file
       file->lines
       first
       (partition size 1)
       (map-indexed vector)
       (some (fn [[i group]]
               (when (= (count (set group)) size)
                 (+ size i))))))

(process 4 "input/day06.example")
(process 4 "input/day06.input")
(process 14 "input/day06.example")
(process 14 "input/day06.input")

;; {::clerk/visibility {:result :show}}
#_(answer-table
   [(partial process 4) (partial process 14)]
   ["input/day06.example" "input/day06.input"]
   (fn [{:keys [result]}]
     result))
