(ns day21.step-counter
  (:require
   [clojure.string :as str]))

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
;; (assert (= 3605 (count (part1 64 (parse input)))))

(defn dims [grid]
  [(inc (apply max (map first (keys grid))))
   (inc (apply max (map second (keys grid))))])

(defn part2 [n grid]
  (let [s (find-start grid)
        [mx my] (dims grid)
        g (assoc grid s \.)]
    (->> [s]
         (iterate (partial steps (fn [[x y]] (g [(mod x mx) (mod y my)]))))
         (take n)
         (map count))))

(defn explore [in n]
  (->> in
       (part2 n)
       (partition 2 1)
       (map (fn [[a b]] [b (- b a)]))
       (map-indexed (fn [i v] [(inc i) v]))))

(dims (parse example))
(dims (parse input))

;; (explore (parse example) 200)
;; (explore (parse input))

;; (assert (= ))
;; (assert (= (part2 (parse input))))
