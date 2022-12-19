(ns aoc.day-19
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 19 :name "example" :content "")
#_(aoc/day-input :day 19)

(defn parse [file]
  (for [blueprint (rest (str/split (slurp file) #"Blueprint "))
        :let [[_ n] (re-find #"(\d+):" blueprint)]]
    [(parse-long n)
     (into {} (for [[_ kind costs] (re-seq #"Each (\w+) robot costs ([^\.]+)\." blueprint)]
                [(keyword kind)
                 (map (fn [[_ x ingredient]] [(parse-long x) (keyword ingredient)])
                      (re-seq #"(\d+) (\w+)" costs))]))]))

(parse "input/day19.example")
(parse "input/day19.input")

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day19")
 (fn [{:keys [result]}] result))
