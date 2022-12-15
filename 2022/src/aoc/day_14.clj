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

(defn fill-empty+floor [grid]
  (let [[[x0 _] [x1 y1]] (grid-range grid)]
    (fill-empty
     (reduce (fn [g l] (assoc g l "#"))
             grid
             (for [x (range (- x0 y1 5) (+ x1 y1 5))]
               [x (+ y1 1)])))))

(defn ->grid [input]
  (reduce (fn [grid polyline]
            (reduce (fn [g l] (assoc g l "#"))
                    grid (coords polyline))) {[500 0] "+"}
          input))

(defn show-grid [grid]
  (let [[[x0 y0] [x1 y1]] (grid-range grid)]
    (into []
          (for [y (range y0 y1)]
            (apply str (for [x (range x0 x1)]
                         (get grid [x y])))))))

(defn v+ [a b] (mapv + a b))

(defn check [grid pos]
  (let [v (get grid pos)]
    (cond (= "." v)
          :open
          (#{"o" "#"} v)
          :blocked
          (nil? v)
          :infinite)))

(defn add-grain [grid]
  (loop [grid grid pos [500 0]]
    (let [below (v+ pos [0 1])
          left (v+ pos [-1 1])
          right (v+ pos [1 1])]
      (cond (= (check grid below) :open)
            (recur grid below)
            (= (check grid below) :infinite)
            grid
            (= (check grid below) :blocked)
            (cond (#{:open :infinite} (check grid left))
                  (recur grid left)
                  (#{:open :infinite} (check grid right))
                  (recur grid right)
                  :else
                  (assoc grid pos "o"))))))

(defn add-till-fixed [grid]
  (reduce
   (fn [g _]
     (let [g' (add-grain g)]
       (if (= g g')
         (reduced g)
         g')))
   grid
   (range)))

(comment
  (let [grid (add-till-fixed (fill-empty (->grid (parse "input/day14.example"))))]
    [(count (filter #{"o"} (vals grid))) (show-grid grid)])
  ;; 1513
  (let [grid (add-till-fixed (fill-empty (->grid (parse "input/day14.input"))))]
    [(count (filter #{"o"} (vals grid))) (show-grid grid)])

  (show-grid (->grid (parse "input/day14.input"))))

(defn star1 [file]
  file)

(comment
  (let [grid (add-till-fixed (fill-empty+floor (->grid (parse "input/day14.example"))))]
    [(count (filter #{"o"} (vals grid))) (show-grid grid)])

  ;; 22646
  (let [grid (add-till-fixed (fill-empty+floor (->grid (parse "input/day14.input"))))]
    [(count (filter #{"o"} (vals grid))) (show-grid grid)]))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day14")
 (fn [{:keys [result]}] result))
