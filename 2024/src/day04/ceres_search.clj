(ns day04.ceres-search
  (:require
   [aoc.grid :as ag]
   [aoc.vector :as v :refer [v* v+ v-]]
   [clojure.string :as str]))

(def input (slurp "src/day04/input"))
(def example (slurp "src/day04/example"))

(defn parse->grid [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn find-starts [grid start]
  (ag/keep-values grid start))

(defn is-xmas? [grid cell dir]
  (and (= (get grid (v+ cell (v* dir 0))) \X)
       (= (get grid (v+ cell (v* dir 1))) \M)
       (= (get grid (v+ cell (v* dir 2))) \A)
       (= (get grid (v+ cell (v* dir 3))) \S)))

(defn part1 [grid]
  (count (for [cell (find-starts grid \X)
               dir v/directions
               :when (is-xmas? grid cell dir)]
           [cell dir])))

(assert (= 18 (part1 (parse->grid example))))
(assert (= 2406 (part1 (parse->grid input))))

(defn mas? [grid cell dir]
  (= #{\M \S}
     (set [(get grid (v+ cell dir))
           (get grid (v- cell dir))])))

(defn x-mas? [grid cell]
  (and (mas? grid cell [1 1])
       (mas? grid cell [1 -1])))

(defn part2 [grid]
  (count (for [cell (find-starts grid \A)
               :when (x-mas? grid cell)]
           cell)))

(assert (= 9 (part2 (parse->grid example))))
(assert (= 1807 (part2 (parse->grid input))))
