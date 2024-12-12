(ns day08.resonant-collinearity
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]
            [aoc.combinatorics :as ac]))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn frequency-nodes [grid]
  (reduce (fn [groups [cell freq]]
            (update groups freq (fnil conj []) cell))
          {}
          (remove (fn [[_ f]] (= f \.)) grid)))

(defn antinodes-pair [grid]
  (fn [[a b]]
    (let [delta (v/v- b a)]
      (filter grid [(v/v- a delta) (v/v+ b delta)]))))

(comment (frequency-nodes (parse example)))

(defn result [antinodes grid]
  (->> grid
       frequency-nodes
       (mapcat (fn [[_ cells]]
                 (mapcat (antinodes grid) (ac/all-pairs cells))))
       distinct
       count))

(assert (= 14 (result antinodes-pair (parse example))))
(assert (= 269 (result antinodes-pair (parse input))))

(defn antinodes-all [grid]
  (fn [[a b]]
    (let [delta (v/v- b a)]
      (concat
       (take-while grid (iterate (fn [x] (v/v- x delta)) a))
       (take-while grid (iterate (fn [x] (v/v+ x delta)) b))))))

(assert
 (= 9 (->> (let [grid (reduce (fn [g [cell v]] (assoc g cell (if (= v \#) \. v))){} (parse (slurp "src/day08/example2")))
                 freqs (get (frequency-nodes grid) \T)]
             (mapcat (antinodes-all grid) (ac/all-pairs freqs)))
           distinct
           count)))

(assert (= 34 (result antinodes-all (parse example))))
(assert (= 949 (result antinodes-all (parse input))))
