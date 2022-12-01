(require '[nextjournal.clerk :as clerk])

(clerk/serve! {:browse? true :watch-paths ["src"]})
(clerk/show! "src/aoc/day_01_calorie_counting.clj")

