(ns hydrothermal-venture
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-lines [filename]
  (->> filename
       slurp
       str/split-lines
       (mapv (fn [line] (map edn/read-string (rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)))))))

(defn line [screen [x0 y0 x1 y1]]
  (cond (= x0 x1)
        (reduce (fn [m y] (update m [x0 y] (fnil inc 0)))
                screen
                (range (min y0 y1) (inc (max y0 y1))))
        (= y0 y1)
        (reduce (fn [m x] (update m [x y0] (fnil inc 0)))
                screen
                (range (min x0 x1) (inc (max x0 x1))))
        :else
        screen))

(defn part1 [filename]
  (->> filename
       parse-lines
       (reduce line {})
       vals
       (remove #(= 1 %))
       count))

(assert (= 5 (part1 "example")))
(assert (= 7380 (part1 "input")))





