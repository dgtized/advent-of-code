(ns user
  (:require [nextjournal.clerk :as clerk]))

(clerk/serve! {:port 7777 :browse? true :watch-paths ["src"]})
(comment
  (clerk/clear-cache!)
  (clerk/show! 'aoc/index))

