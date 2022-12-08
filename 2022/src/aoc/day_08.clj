(ns aoc.day-08
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}
(defn v+ [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn v- [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(def dirs
  {:n [0 -1]
   :s [0 1]
   :e [1 0]
   :w [-1 0]})

(defn file->grid [file]
  (for [[j line] (map-indexed vector (aoc/file->lines file))
        [i tree] (map-indexed vector line)]
    {:loc [i j] :tree (parse-long (str tree)) :visible #{:n :e :s :w}}))

(defn positions [grid start dir]
  (->> start
       (iterate (fn [p] (v+ p dir)))
       (take-while (fn [p] (contains? grid p)))))

(defn scan [grid start dir dir-name]
  (reduce (fn [[g occlude] p]
            (let [tree (:tree (get grid p))]
              [(if (> tree occlude)
                 g
                 (update g p update :visible disj dir-name))
               (max tree occlude)]))
          [grid -1]
          (positions grid start dir)))

(defn indexed-grid [file]
  (let [grid (file->grid file)]
    (zipmap (map :loc grid) (map #(dissoc % :loc) grid))))

(comment
  (positions (indexed-grid "input/day08.example") [0 0] [0 1])
  (println)
  (scan (indexed-grid "input/day08.example") [0 0] [0 1] :s)
  (scan (indexed-grid "input/day08.example") [0 0] [1 0] :e))

(defn occluded [grid]
  (let [row (apply max (map second (keys grid)))
        col (apply max (map first (keys grid)))
        directions {:s (positions grid [0 0] [1 0])
                    :n (positions grid [0 row] [1 0])
                    :e (positions grid [0 0] [0 1])
                    :w (positions grid [col 0] [0 1])}]
    (reduce (fn [g [d starts]]
              (reduce (fn [g' start] (first (scan g' start (get dirs d) d)))
                      g starts))
            grid directions)))

(defn part1 [file]
  (let [grid (indexed-grid file)
        occluded (occluded grid)
        not-occluded (remove (fn [[_ {:keys [visible]}]] (empty? visible)) occluded)]
    [(count not-occluded) #_occluded]))

(defn part2 [file]
  [])

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [part1 part2]
 (aoc/input-files "day08")
 (fn [{:keys [result]}] result))
