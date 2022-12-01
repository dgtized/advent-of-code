(ns aoc.index
  "[Advent Of Clerk](https://github.com/nextjournal/advent-of-clerk/blob/main/src/advent_of_clerk/index.clj)"
  {:nextjournal.clerk/visibility {:code :hide :result :hide}}
  (:require [nextjournal.clerk :as clerk]
            [babashka.fs :as fs]))

(defn build-paths []
  (map str (fs/glob "src" "**.clj")))

#_(clerk/doc-url (str (fs/file "src" "aoc" "day_01_calorie_counting.clj")))
#_(build-paths)

{:nextjournal.clerk/visibility {:result :show}}

;; FIXME links are not changing active notebook
^::clerk/no-cache
(clerk/html
 (into [:ul]
       (for [path (build-paths)]
         [:li [:a {:href (clerk/doc-url path)} path]])))
