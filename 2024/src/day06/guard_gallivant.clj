(ns day06.guard-gallivant
  (:require [aoc.grid :as ag]
            [clojure.string :as str]
            [aoc.vector :as v]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn clockwise [face]
  (->> v/cardinal
       cycle
       (drop-while (fn [dir] (not= dir face)))
       (drop 1)
       first))

(comment (clockwise [1 0])
         (clockwise [0 1])
         (clockwise [-1 0])
         (clockwise [0 -1]))

(defn step [[grid pos dir]]
  (let [pos' (v/v+ pos dir)]
    (case (get grid pos')
      \# [grid pos (clockwise dir)]
      \. [(assoc grid pos \X) pos' dir]
      \X [(assoc grid pos \X) pos' dir]
      nil [(assoc grid pos \X)])))

(defn walk [grid]
  (let [start (some (fn [[cell v]] (when (= v \^) cell)) grid)]
    (ffirst (drop-while (fn [[grid pos _]] (not= :outside (get grid pos :outside)))
                        (iterate step [grid start [0 -1]])))))

(defn part1 [in]
  (count (filter #(= % \X) (vals (walk in)))))

(assert (= 41 (part1 (parse example))))
(assert (= 5239 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
