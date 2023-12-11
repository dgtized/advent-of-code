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

(defn all-pairs-distance [galaxies]
  (apply + (for [[a b] (all-pairs galaxies)]
             (manhattan a b))))

(defn part1 [in]
  (let [grid (->grid (expand in))
        galaxies (keep (fn [[p v]] (when (= v \#) p)) grid)]
    (all-pairs-distance galaxies)))

(assert (= 374 (part1 (parse example))))
(assert (= 9605127 (part1 (parse input))))

(defn part2 [mult in]
  (let [grid (->grid in)
        exp-rows (keep (fn [[i r]] (when (every? #{\.} r)
                                    i))
                       (map-indexed vector in))
        exp-cols (keep (fn [[i r]] (when (every? #{\.} r)
                                    i))
                       (map-indexed vector (transpose in)))
        galaxies (keep (fn [[p v]] (when (= v \#) p)) grid)]
    (for [[x y] galaxies
          :let [cols (filter #(< % x) exp-cols)
                rows (filter #(< % y) exp-rows)]]
      [(+ x (* (dec mult) (count cols)))
       (+ y (* (dec mult) (count rows)))])))

(assert (= 1030 (all-pairs-distance (part2 10 (parse example)))))
(assert (= 8410 (all-pairs-distance (part2 100 (parse example)))))
(assert (= 458191688761 (all-pairs-distance (part2 1000000 (parse input)))))
