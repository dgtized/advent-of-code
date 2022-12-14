(ns aoc.day-14
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 14 :name "example" :content "")
#_(aoc/day-input :day 14)

(defn parse [file]
  (map (fn [s]
         (map (comp (fn [v] (mapv parse-long (str/split v #","))) str/trim)
              (str/split s #"->")))
       (aoc/file->lines file)))

(parse "input/day14.example")
(parse "input/day14.input")

(defn s-range [a b]
  (if (> a b)
    (reverse (range (inc b) (inc a) 1))
    (range a b 1)))

(comment (s-range 4 6)
         (s-range 6 4))

(defn coords [polyline]
  (concat (mapcat (fn [[[x0 y0] [x1 y1]]]
                    (if (= x0 x1)
                      (mapv (fn [y] [x1 y]) (s-range y0 y1))
                      (mapv (fn [x] [x y1]) (s-range x0 x1))))
                  (partition 2 1 polyline))
          [(last polyline)]))

(comment
  (= (coords [[0 0] [0 2] [2 2] [2 0] [0 0]])
     [[0 0] [0 1] [0 2]
      [1 2] [2 2]
      [2 1] [2 0]
      [1 0] [0 0]])
  (= (coords [[498 4] [498 6] [496 6]])
     [[498 4] [498 5] [498 6] [497 6] [496 6]])
  (= (coords [[498 4] [498 6] [496 6] [496 8]])
     [[498 4] [498 5] [498 6] [497 6] [496 6] [496 7] [496 8]]))

(defn ->grid [input]
  (reduce (fn [grid polyline] (reduce (fn [g l] (assoc g l "#"))
                                     grid (coords polyline))) {[500 0] "+"} input))

(defn show-grid [grid]
  (let [xs (map first (keys grid))
        x0 (apply min xs)
        x1 (inc (apply max xs))
        ys (map second (keys grid))
        y0 (apply min ys)
        y1 (inc (apply max ys))]
    (into []
          (for [y (range y0 y1)]
            (apply str (for [x (range x0 x1)]
                         (get grid [x y] ".")))))))

(show-grid (->grid (parse "input/day14.example")))
(show-grid (->grid (parse "input/day14.input")))

(defn star1 [file]
  (parse file))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day14")
 (fn [{:keys [result]}] result))
