(ns aoc.day-04
  (:require
   [aoc.utility :refer [file->lines verify]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

(defn parse [input]
  (->> input
       (map (fn [line]
              (mapcat (fn [part] (map parse-long (str/split part #"-"))) (str/split line #","))))
       (filter (fn [[a b c d]]
                 (or (and (<= a c) (<= d b))
                     (and (<= c a) (<= b d)))))))

(clerk/table
 {:head ["input" "star" "answer"]
  :rows
  (for [[star method] {1 parse
                       2 identity}
        file ["input/day04.example" "input/day04.input"]]
    (let [result (method (file->lines file))]
      [file star (count result) result]))})
