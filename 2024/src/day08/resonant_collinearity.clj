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
  (->> cells
       all-pairs
       (mapcat (fn [[a b]]
                 (let [delta (v/v- b a)]
                   [(v/v- a delta) (v/v+ b delta)])))
       (filter grid)))

(comment (frequencies (parse example)))

(defn result [antinodes grid]
  (->> grid
       frequencies
       (mapcat (fn [[_ cells]] (antinodes grid cells)))
       distinct
       count))

(assert (= 14 (result antinodes (parse example))))
(assert (= 269 (result antinodes (parse input))))

(defn antinodes-overlap [grid cells]
  (->> cells
       all-pairs
       (mapcat (fn [[a b]]
                 (let [delta (v/v- b a)]
                   (concat
                    (take-while grid (iterate (fn [x] (v/v- x delta)) a))
                    (take-while grid (iterate (fn [x] (v/v+ x delta)) b))))))))

(assert
 (= 9 (->> (let [grid (reduce (fn [g [cell v]] (assoc g cell (if (= v \#) \. v))){} (parse (slurp "src/day08/example2")))
                 freqs (get (frequencies grid) \T)]
             (antinodes-overlap grid freqs))
           distinct
           count)))

(assert (= 34 (result antinodes-overlap (parse example))))
(assert (= 949 (result antinodes-overlap (parse input))))
