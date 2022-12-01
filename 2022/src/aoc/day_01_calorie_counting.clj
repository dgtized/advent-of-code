(ns aoc.day-01-calorie-counting
  (:require [clojure.string :as str]))

(defn parse [filename]
  (str/split-lines (slurp filename)))

;; first star
(->> "day01.in.txt"
     parse
     (map (fn [s] (if-not (= s "") (parse-long s) s)))
     (partition-by (fn [s] (= "" s)))
     (remove (fn [group] (= [""] group)))
     (map (fn [group] (reduce + group)))
     sort
     last)

;; second star
(->> "day01.in.txt"
     parse
     (map (fn [s] (if-not (= s "") (parse-long s) s)))
     (partition-by (fn [s] (= "" s)))
     (remove (fn [group] (= [""] group)))
     (map (fn [group] (reduce + group)))
     sort
     (take-last 3)
     (reduce +))


