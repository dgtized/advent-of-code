(ns day14.restroom-redoubt
  (:require [clojure.string :as str]
            [aoc.vector :as v]))

(def input (slurp "src/day14/input"))
(def example (slurp "src/day14/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [line] (map vec (partition 2 (map parse-long (re-seq #"-?\d+" line))))))))

(defn bounds [[x-max y-max] [x y]]
  [(mod x x-max) (mod y y-max)])

(defn move [bounds [pos vel]]
  [(bounds (v/v+ pos vel)) vel])

(defn step [size robots n]
  (nth (iterate (fn [robots] (mapv (partial move (partial bounds size)) robots))
                robots)
       n))

(defn quadrants [[mx my] robots]
  (for [[[x0 y0] [x1 y1]]
        [[[0 0] [(dec (int (/ mx 2))) (dec (int (/ my 2)))]]
         [[(inc (int (/ mx 2))) 0] [mx (dec (int (/ my 2)))]]
         [[0 (inc (int (/ my 2)))] [(dec (int (/ mx 2))) my]]
         [[(inc (int (/ mx 2))) (inc (int (/ my 2)))] [mx my]]]]
    (filter (fn [[[x y] _]] (and (<= x0 x x1) (<= y0 y y1))) robots)))

(defn part1 [robots size]
  (apply * (map count (quadrants size (step size robots 100)))))

(assert (= 12 (part1 (parse example) [11 7])))
(assert (= 231019008 (part1 (parse input) [101 103])))

(defn make-grid [robots]
  (reduce (fn [grid [pos _]]
            (update grid pos (fnil inc 0)))
          {} robots))

(defn display [robots [mx my]]
  (let [grid (make-grid robots)]
    (for [y (range my)]
      (apply str (for [x (range mx)]
                   (get grid [x y] 0))))))

(defn part2 [robots size]
  (let [[step robots']
        (some
         (fn [[step robots]]
           #_(when (zero? (mod step 1000)) (println step))
           (let [grid (make-grid robots)]
             (when (or (and (> step 1) (= [1] (distinct (vals grid)))
                            (re-find #"11111111" (apply str (display robots size))))
                       (> step 10000))
               [step robots])))
         (iterate (fn [[steps robots]]
                    [(inc steps) (mapv (partial move (partial bounds size)) robots)])
                  [0 robots]))]
    [step (display robots' size)]))

;; (assert (= (part2 (parse example) [11 7])))
(assert (= 8280 (first (part2 (parse input) [101 103]))))
