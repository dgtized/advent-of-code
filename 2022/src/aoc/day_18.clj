(ns aoc.day-18
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 18 :name "example" :content "")
#_(aoc/day-input :day 18)

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (mapv parse-long (str/split line #","))))

(def example (parse "input/day18.example"))
(def input (parse "input/day18.input"))

(defn v+ [a b] (mapv + a b))

(defn faces [coord]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (= (+ (Math/abs dx) (Math/abs dy) (Math/abs dz)) 1)]
    (v+ coord [dx dy dz])))

#_(faces [0 0 0])
#_(faces (first example))

(defn exposed-faces [index coord]
  (->> coord
       faces
       (remove (fn [face] (contains? index face)))))

#_(faces [1 1 1])
#_(exposed-faces (set [[1 1 1] [2 1 1]]) [1 1 1])

(defn star1 [file]
  (let [cubes (parse file)
        index (set cubes)]
    (reduce + (map (fn [c] (count (exposed-faces index c))) cubes))))

#_(star1 "input/day18.example")

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day18")
 (fn [{:keys [result]}] result))
