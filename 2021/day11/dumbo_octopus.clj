(ns dumbo-octopus
  (:require [clojure.string :as str]))

(defn parse [filename]
  (mapv (fn [line] (mapv (comp parse-long str) (seq line)))
        (str/split-lines (slurp filename))))

(defn grid->map
  "Change grid into a map of coords -> value."
  [grid]
  (into {}
        (for [j (range 10)
              i (range 10)]
          {[i j] (nth (nth grid j) i)})))

(defn map->grid
  "Convert map from back into a grid for easier debug output."
  [map]
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
          grid flashes))

(defn apply-flashes [grid]
  (let [flashes (to-flash grid)]
    (if (empty? flashes)
      grid
      (recur (-> (zero grid flashes)
                 (propagate (mapcat neighbors flashes)))))))

(defn step [grid]
  (-> grid
      grid->map
      (update-vals inc)
      apply-flashes
      map->grid))

(assert (= (parse "step100") (last (take 101 (iterate step (parse "example"))))))
(assert (= (parse "example") (map->grid (grid->map (parse "example")))))
(comment (step (step (step (parse "example")))))

(defn count-flashes [grid]
  (->> grid
       flatten
       (filter zero?)
       count))

(defn part1 [grid]
  (->> grid
       (iterate step)
       (take 101)
       (mapv count-flashes)
       (apply +)))

;; Lazily count flashes at each step until they all flash
(defn part2 [grid]
  (->> grid
       (iterate step)
       (map count-flashes)
       (take-while (fn [n] (< n 100)))
       count))

(assert (= 1656 (part1 (parse "example"))))
(assert (= 1585 (part1 (parse "input"))))

(assert (= 195 (part2 (parse "example"))))
(assert (= 382 (part2 (parse "input"))))
