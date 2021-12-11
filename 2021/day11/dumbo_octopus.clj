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
  (assoc grid coord 0))

(defn propagate [grid coord]
  (if (= (get grid coord) 0)
    grid
    (update grid coord inc)))

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

(assert (= [[0 3 9 7 6 6 6 8 6 6]
            [0 7 4 9 7 6 6 9 1 8]
            [0 0 5 3 9 7 6 9 3 3]
            [0 0 0 4 2 9 7 8 2 2]
            [0 0 0 4 2 2 9 8 9 2]
            [0 0 5 3 2 2 2 8 7 7]
            [0 5 3 2 2 2 2 9 6 6]
            [9 3 2 2 2 2 8 9 6 6]
            [7 9 2 2 2 8 6 8 6 6]
            [6 7 8 9 9 9 8 7 6 6]]
           (iterate-cycles 100 step (parse "example"))))

(assert (= 1656 (part1 (parse "example"))))
(assert (= 1585 (part1 (parse "input"))))

(assert (= 195 (part2 (parse "example"))))
(assert (= 382 (part2 (parse "input"))))
