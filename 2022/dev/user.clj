(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:port 7777 :browse? true :watch-paths ["src"]})
(comment
  (clerk/show! 'aoc/index)
  (clerk/show! 'aoc/day-01-calorie-counting)
  (clerk/show! 'aoc/day-02))

