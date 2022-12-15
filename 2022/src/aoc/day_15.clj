(ns aoc.day-15
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 15 :name "example" :content "")
#_(aoc/day-input :day 15)

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (->> line
         (re-seq #"(-?\d+)")
         (map (comp parse-long first)))))

(defn ->grid [input]
  (reduce (fn [g [sx sy bx by]]
            (assoc g [sx sy] "S"
                   [bx by] "B"))
          {}
          input))

(defn grid-range [grid]
  (let [xs (map first (keys grid))
        x0 (apply min xs)
        x1 (inc (apply max xs))
        ys (map second (keys grid))
        y0 (apply min ys)
        y1 (inc (apply max ys))]
    [[x0 y0] [x1 y1]]))

(defn fill-empty [grid]
  (let [[[x0 y0] [x1 y1]] (grid-range grid)]
    (reduce (fn [g l]
              (if (contains? g l)
                g
                (assoc g l ".")))
            grid
            (for [x (range x0 x1)
                  y (range y0 y1)]
              [x y]))))

(defn show-grid [grid]
  (let [[[x0 y0] [x1 y1]] (grid-range grid)]
    (into []
          (for [y (range y0 y1)]
            (apply str (for [x (range x0 x1)]
                         (get grid [x y])))))))

#_(show-grid (fill-empty (->grid (parse "input/day15.example"))))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day15")
 (fn [{:keys [result]}] result))
