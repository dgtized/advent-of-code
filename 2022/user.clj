(require '[nextjournal.clerk :as clerk])

(clerk/serve! {:browse? true})
(clerk/show! "src/aoc/day_01.clj")
(clerk/serve! {:watch-paths ["src"]})
