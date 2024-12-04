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

(defn find-starts [grid start]
  (filter (fn [[_ value]] (= value start)) grid))

(def directions (for [i [-1 0 1] j [-1 0 1]
                      :when (not= 0 i j)]
                  [i j]))

(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn v* [v n]
  (mapv #(* % n) v))

(defn is-xmas? [grid cell dir]
  (and (= (get grid (v+ cell (v* dir 0))) \X)
       (= (get grid (v+ cell (v* dir 1))) \M)
       (= (get grid (v+ cell (v* dir 2))) \A)
       (= (get grid (v+ cell (v* dir 3))) \S)))

(defn part1 [grid]
  (count (for [[cell _] (find-starts grid \X)
               dir directions
               :when (is-xmas? grid cell dir)]
           [cell dir])))

(assert (= 18 (part1 (parse->grid example))))
(assert (= 2406 (part1 (parse->grid input))))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn mas? [grid cell dir]
  (= #{\M \S}
     (set [(get grid (v+ cell dir))
           (get grid (v- cell dir))])))

(defn x-mas? [grid cell]
  (and (mas? grid cell [1 1])
       (mas? grid cell [1 -1])))

(defn part2 [grid]
  (count (for [[cell _] (find-starts grid \A)
               :when (x-mas? grid cell)]
           cell)))

(assert (= 9 (part2 (parse->grid example))))
(assert (= 1807 (part2 (parse->grid input))))
