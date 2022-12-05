(ns aoc.day-05
  {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

(defn transpose [xs]
  (apply map vector xs))

(defn parse [file]
  (let [[stacks moves]
        (split-with (fn [line] (not= line ""))
                    (file->lines file))]
    [(->> stacks
          butlast
          (map (fn [line] (mapv str (str/replace (str/replace line #"\s\s\s" "_") #"[\[\]\s+]" ""))))
          transpose
          (mapv (comp reverse (partial remove #{"_"}))))
     (map (fn [line] (re-seq #"\d+" line)) (rest moves))]))

{::clerk/visibility {:result :show}}
(answer-table
 [parse]
 ["input/day05.example" "input/day05.input"]
 (fn [{:keys [result]}]
   [(count result) result]))
