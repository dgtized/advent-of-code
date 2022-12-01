(ns aoc.day-01-calorie-counting
  (:require [clojure.string :as str]))

(defn parse-groups [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [s] (if-not (= s "") (parse-long s) s)))
       (partition-by (fn [s] (= "" s)))
       (remove (fn [group] (= [""] group)))))

;; ### First Star
(->> "input/day01.in.txt"
     parse-groups
     (map (partial apply +))
     sort
     last)

;; ### Second Star
(->> "input/day01.in.txt"
     parse-groups
     (map (partial apply +))
     sort
     (take-last 3)
     (reduce +))


