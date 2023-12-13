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
           (map vector (reverse before) after)
           (map vector before (reverse after)))
         (every? (fn [[a b]] (= a b))))))

(defn folds [s]
  (->> (range 1 (count s))
       (keep (fn [k] (when (mirrored-at? s k) k)))
       set))

(defn mirrors [example]
  (apply set/intersection (map folds example)))

(defn transpose [m]
  (map #(apply str %) (apply mapv vector m)))

(mirrored-at? "#.#.#" 2)
(mirrored-at? "#.##.#" 2)
(folds "#.#.#")
(folds "#.##.#")
(folds "#.##..##.")

(defn part1 [in]
  (for [grid in]
    (if-let [v (first (mirrors grid))]
      v
      (if-let [h (first (mirrors (transpose grid)))]
        (* 100 h)
        0))))

((juxt identity mirrors
       transpose (comp mirrors transpose))
 (nth (parse example) 1))

(assert (= 405 (part1 (parse example))))
;; not = 12715
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
