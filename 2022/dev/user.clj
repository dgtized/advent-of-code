(ns user
  (:require [nextjournal.clerk :as clerk]
            ))

(clerk/serve! {:port 7777 :browse? true :watch-paths ["src"]})
(comment
  (clerk/show! "src/aoc/day_01_calorie_counting.clj"))

