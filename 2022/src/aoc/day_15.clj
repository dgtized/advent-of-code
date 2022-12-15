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
         (map (comp parse-long first))
         (partition 2))))

(defn ->grid [input]
  (reduce (fn [g [[sx sy] [bx by]]]
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

(defn manhattan [[[x0 y0] [x1 y1]]]
  (+ (Math/abs (- x1 x0)) (Math/abs (- y1 y0))))

#_(show-grid (fill-empty (->grid (parse "input/day15.example"))))

(defn occluded [[x0 y0] dist row]
  (let [center [x0 row]
        center-dist (manhattan [center [x0 y0]])]
    (cond (> center-dist dist)
          []
          (= center-dist dist)
          [x0 x0]
          :else
          (let [delta (Math/abs (- center-dist dist))]
            [(- x0 delta) (+ x0 delta)]))))

(comment (occluded [5 0] 5 0)
         (occluded [5 0] 5 4)
         (occluded [5 0] 5 5))

(defn sweep [ranges]
  (let [rs (remove empty? ranges)]
    (reverse
     (reduce (fn [acc r]
               (if (empty? acc)
                 (cons r acc)
                 (let [[a b] (first acc)
                       [c d] r]
                   (if (< b c)
                     (cons [c d] acc)
                     (cons [(min a c) (max b d)] (rest acc))))))
             '()
             rs))))

(defn count-ranges [ranges]
  (apply + (map (fn [[a b]] (- b a)) ranges)))

(comment (sweep [[0 1] [] [1 2] [2] [4 5]]))

(defn count-range [r]
  (case (count r)
    0 0
    1 1
    2 (- (second r) (first r))))

(defn star1 [file row]
  (let [sensor-beacons (parse file)
        sensors (into {} (map (juxt first manhattan) sensor-beacons))
        xs (map first (apply concat sensor-beacons))
        fx [(apply min xs) row]
        lx [(apply max xs) row]]
    {:range [fx lx]
     :sensors (mapv (fn [sensor] [sensor (get sensors sensor) ((juxt identity count-range) (occluded sensor (get sensors sensor) row))])
                    (sort-by (fn [sensor] (manhattan [fx sensor])) (map first sensor-beacons)))
     ;; :beacons (sort-by (fn [sensor] (manhattan [fx sensor])) (map second sensor-beacons))
     :ranges (count-ranges (sweep (mapv (fn [sensor] (occluded sensor (get sensors sensor) row))
                                        (sort-by (fn [sensor] (manhattan [fx sensor])) (map first sensor-beacons)))))}))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(star1 "input/day15.example" 10)
(star1 "input/day15.input" 2000000) ;; 4665948
