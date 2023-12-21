(ns day21.step-counter
  (:require [clojure.string :as str]))

(def input (slurp "src/day21/input"))
(def example (slurp "src/day21/example"))

(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn v+ [a b] (mapv + a b))

(defn parse [in]
  (->grid (str/split-lines in)))

(defn find-start [in]
  (some (fn [[k v]] (when (= v \S) k)) in))

(defn neighbors [lookup pos]
  (for [dir [[-1 0] [1 0] [0 1] [0 -1]]
        :let [n (v+ pos dir)]
        :when (= \. (lookup n))]
    n))

(defn steps [lookup positions]
  (->> positions
       (mapcat (fn [p] (neighbors lookup p)))
       distinct))

(defn part1 [n grid]
  (let [s (find-start grid)
        g (assoc grid s \.)]
    (nth (iterate (partial steps g) [s]) n)))

(assert (= 16 (count (part1 6 (parse example)))))
(assert (= 3605 (count (part1 64 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
