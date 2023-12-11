(ns day11.cosmic-expansion
  (:require [clojure.string :as str]))


(def input (slurp "src/day11/input"))
(def example (slurp "src/day11/example"))


(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn parse [in]
  (str/split-lines in))

(defn transpose [m]
  (apply mapv vector m))

(defn double-rows [rows]
  (mapcat (fn [r]
            (if (every? #{\.} r)
              [r r]
              [r]))
          rows))

(defn expand [rows]
  (->> rows
       double-rows
       transpose
       double-rows
       transpose))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn manhattan [[x0 y0] [x1 y1]]
  (+ (Math/abs (- x1 x0))
     (Math/abs (- y1 y0))))

(defn part1 [in]
  (let [grid (->grid (expand in))
        galaxies (keep (fn [[p v]] (when (= v \#) p)) grid)]
    (apply + (for [[a b] (all-pairs galaxies)]
               (manhattan a b)))))

(assert (= 374 (part1 (parse example))))
(assert (= 9605127 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
