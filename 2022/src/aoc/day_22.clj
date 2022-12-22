(ns aoc.day-22
  (:require
   [aoc.utility :as aoc]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 22 :name "example" :content "")
#_(aoc/day-input :day 22)

(defn parse-grid [grid]
  (into {}
        (for [[j line] (map-indexed vector (str/split-lines grid))
              [i c] (map-indexed vector (seq line))
              :when (contains? #{\# \.} c)]
          [[i j] c])))

(defn parse [file]
  (let [[grid dirs] (aoc/split-empty-lines (slurp file))]
    {:grid (parse-grid grid)
     :path (->> dirs
                (re-seq #"([RL]|\d+)")
                (mapv (fn [[_ m]] (or (parse-long m) m))))}))

(def example (parse "input/day22.example"))
(def input (parse "input/day22.input"))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day22")
 (fn [{:keys [result]}] result))
