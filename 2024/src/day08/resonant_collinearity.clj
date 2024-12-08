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

(defn antinodes [cells]
  (mapcat (fn [[a b]]
            (let [delta (v/v- b a)]
              [(v/v- a delta) (v/v+ b delta)]
              ))
          (all-pairs cells)))

(comment (frequencies (parse example)))

(defn part1 [grid]
  (->> grid
       frequencies
       (mapcat (fn [[_ cells]] (antinodes cells)))
       (filter grid)
       distinct
       count))

(assert (= 14 (part1 (parse example))))
(assert (= 269 (part1 (parse input))))

(defn antinodes-overlap [grid cells]
  (->> cells
       all-pairs
       (mapcat (fn [[a b]]
                 (let [delta (v/v- b a)]
                   (concat
                    (take-while grid (iterate (fn [x] (v/v- x delta)) a))
                    (take-while grid (iterate (fn [x] (v/v+ x delta)) b))))))))

(comment
  (let [grid (reduce (fn [g [cell v]] (assoc g cell (if (= v \#) \. v))){} (parse (slurp "src/day08/example2")))
        freqs (get (frequencies grid) \T)]
    (antinodes-overlap grid freqs)))

(defn part2 [grid]
  (->> grid
       frequencies
       (mapcat (fn [[_ cells]] (antinodes-overlap grid cells)))
       distinct
       count))

(assert (= 34 (part2 (parse example))))
(assert (= 949 (part2 (parse input))))
