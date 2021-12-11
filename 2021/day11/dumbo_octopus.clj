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

(defn zero [grid flashes]
  (reduce (fn [g c] (assoc g c 0))
          grid flashes))

(defn propagate [grid flashes]
  (reduce (fn [g c]
            (if (zero? (get g c))
              g
              (update g c inc)))
          grid
          (mapcat neighbors flashes)))

(defn apply-flashes [grid]
  (let [flashes (to-flash grid)]
    (if (empty? flashes)
      grid
      (recur (-> grid
                 (zero flashes)
                 (propagate flashes))))))

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

(defn count-flashes [grid]
  (count (filter zero? (vals (grid->map grid)))))

(defn part1 [grid]
  (apply + (mapv count-flashes (take 101 (iterate step grid)))))

(defn part2 [grid]
  (->> grid
       (iterate step)
       (map count-flashes)
       (take-while (fn [n] (< n 100)))
       count))

(assert (= (parse "step100") (iterate-cycles 100 step (parse "example"))))

(assert (= 1656 (part1 (parse "example"))))
(assert (= 1585 (part1 (parse "input"))))

(assert (= 195 (part2 (parse "example"))))
(assert (= 382 (part2 (parse "input"))))
