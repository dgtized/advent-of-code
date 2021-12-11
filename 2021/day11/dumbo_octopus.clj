(ns dumbo-octopus
  (:require [clojure.string :as str]))

(defn parse [filename]
  (mapv (fn [line] (mapv (comp parse-long str) (seq line)))
        (str/split-lines (slurp filename))))

(defn grid->map [grid]
  (into {}
        (for [j (range 10)
              i (range 10)]
          {[i j] (nth (nth grid j) i)})))

(defn map->grid [map]
  (->> (for [j (range 10)
             i (range 10)]
         (get map [i j]))
       (partition 10)
       (mapv vec)))

(defn neighbors [[x y]]
  (filter (fn [[x y]] (and (<= 0 x 9) (<= 0 y 9)))
          (for [j [-1 0 1]
                i [-1 0 1]
                :when (not= i j 0)]
            [(+ x i) (+ y j)])))

(assert (= [[0 0] [2 0] [0 1] [1 1] [2 1]] (neighbors [1 0])))

(defn to-flash [grid]
  (keep (fn [[coord value]]
          (when (> value 9) coord))
        grid))

(defn flash [grid coord]
  (let [v (get grid coord)]
    (if (> v 9)
      (assoc grid coord 0)
      grid)))

(defn propagate [grid coord]
  (let [v (get grid coord)]
    (cond (= v 0)
          grid
          :else
          (update grid coord inc))))

(defn apply-flashes [grid]
  (let [flashes (to-flash grid)]
    (if (empty? flashes)
      grid
      (recur (reduce propagate
                     (reduce flash grid flashes)
                     (mapcat neighbors flashes))))))

(defn iterate-cycles
  [n f x]
  (last (take (inc n) (iterate f x))))

(defn step [grid]
  (as-> grid g
    (grid->map g)
    (update-vals g inc)
    (apply-flashes g)
    (map->grid g)))

(comment (map->grid (grid->map (parse "example")))
         (step (step (step (parse "example")))))

(iterate-cycles 100 step (parse "example"))



;; (apply-flashes (apply-flashes (update-vals (grid->map (step (parse "example"))) inc)))
