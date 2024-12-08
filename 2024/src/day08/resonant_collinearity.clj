(ns day08.resonant-collinearity
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn frequencies [grid]
  (reduce (fn [groups [cell freq]]
            (update groups freq (fnil conj []) cell))
          {}
          (remove (fn [[_ f]] (= f \.)) grid)))

(defn antinodes [grid cells]
  (mapcat (fn [[a b]]
            (let [delta (v/v- b a)]
              [(v/v- a delta) (v/v+ b delta)]
              ))
          (all-pairs cells)))

(comment (frequencies (parse example)))

(defn part1 [grid]
  (->> grid
       frequencies
       (mapcat (fn [[freq cells]] (antinodes grid cells)))
       (filter grid)
       distinct
       count))

(assert (= 14 (part1 (parse example))))
(assert (= 269 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
