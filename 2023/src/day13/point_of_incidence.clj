(ns day13.point-of-incidence
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day13/input"))
(def example (slurp "src/day13/example"))

(defn parse [in]
  (map str/split-lines (str/split in #"\n\n")))

(defn mirrored-at? [s k]
  (let [[before after] (split-at k s)]
    (->> (if (> (count before) (count after))
           (map = after (reverse before))
           (map = (reverse before) after))
         (every? true?))))

(defn folds [s]
  (->> (range 1 (count s))
       (keep (fn [k] (when (mirrored-at? s k) k)))
       set))

(defn mirrors [example]
  (apply set/intersection (map folds example)))

(defn rotate [m]
  (reverse (apply mapv str m)))

(rotate ["123"
         "456"
         "789"])

(mirrored-at? "#.#.#" 2)
(mirrored-at? "#.##.#" 2)
(folds "#.#.#")
(folds "#.##.#")
(folds "#.##..##.")

(defn scores [grid]
  (concat
   (mirrors grid)
   (map (partial * 100) (mirrors (rotate grid)))))

(defn part1 [in]
  (apply + (map (comp first scores) in)))

(map (comp folds) (nth (parse input) 1))
(split-at 9 "..#.##.#..#.##...")

((juxt identity mirrors
       rotate (comp mirrors rotate))
 (nth (parse input) 2))

(assert (= 405 (part1 (parse example))))
(assert (= 42974 (part1 (parse input))))

(defn perturbations [grid]
  (let [flat (vec (mapcat vec grid))]
    (for [i (range (count flat))]
      (->> (update flat i {\# \. \. \#})
           (partition (count (first grid)))
           (mapv (partial apply str))))))

(perturbations ["#." ".#"])

(defn alternate [grid]
  (let [orig (set (scores grid))]
    (some (fn [g] (let [s (set (scores g))
                       d (set/difference s orig)]
                   (when (seq d) (first d))))
          (perturbations grid))))

(alternate (nth (parse example) 0))
(alternate (nth (parse example) 1))

(defn part2 [in]
  (apply + (map alternate in)))

(assert (= 400 (part2 (parse example))))
(assert (= 27587 (part2 (parse input))))
