(ns day04.day04
  (:require [clojure.string :as str]))

(def input (slurp "src/day04/input"))
(def example (slurp "src/day04/example"))

(defn parse->grid [in]
  (into {}
        (let [lines (str/split-lines in)]
          (mapcat (fn [j line]
                    (map-indexed (fn [i c] [[i j] c]) line))
                  (range (count lines)) lines))))

(defn find-xs [grid]
  (let [starts (filter (fn [[_ value]] (= value \X)) grid)]
    starts))

(def directions (for [i [-1 0 1] j [-1 0 1]
                      :when (not= 0 i j)]
                  [i j]))

(defn vplus [v1 v2]
  (map + v1 v2))

(defn vtimes [[a b] val]
  [(* a val) (* b val)])

(defn is-xmas? [grid cell dir]
  (and (= (get grid (vplus cell (vtimes dir 0))) \X)
       (= (get grid (vplus cell (vtimes dir 1))) \M)
       (= (get grid (vplus cell (vtimes dir 2))) \A)
       (= (get grid (vplus cell (vtimes dir 3))) \S)))

(defn part1 [grid]
  (count (for [[cell _] (find-xs grid)
               dir directions
               :when (is-xmas? grid cell dir)]
           [cell dir])))

(assert (= 18 (part1 (parse->grid example))))
(assert (= 2406 (part1 (parse->grid input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse->grid example))))
(assert (= (part2 (parse->grid input))))
