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

(defn score [in]
  (for [grid in]
    (if-let [v (first (mirrors grid))]
      v
      (if-let [h (first (mirrors (rotate grid)))]
        (* 100 h)
        0))))

(defn part1 [in]
  (apply + (score in)))

(map (comp folds) (nth (parse input) 1))
(split-at 9 "..#.##.#..#.##...")

((juxt identity mirrors
       rotate (comp mirrors rotate))
 (nth (parse input) 2))

(assert (= 405 (part1 (parse example))))
(assert (= 42974 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
