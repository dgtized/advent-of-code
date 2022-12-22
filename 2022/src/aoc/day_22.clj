(ns aoc.day-22
  (:require
   [aoc.utility :as aoc]
   [clojure.set :as set]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 22 :name "example" :content "")
#_(aoc/day-input :day 22)

(defn parse-grid [grid]
  (into {}
        (for [[j line] (map-indexed vector (str/split-lines grid))
              [i c] (map-indexed vector (seq line))
              :when (contains? #{\# \.} c)]
          [[i j] c])))

(defn parse [file]
  (let [[grid-str dirs] (aoc/split-empty-lines (slurp file))
        grid (parse-grid grid-str)]
    {:grid grid
     :start (let [row (first (str/split-lines grid-str))]
              (some (fn [c] (when (= (get grid c nil) \.) c))
                    (map (fn [i] [i 0]) (range (count row)))))
     :path (->> dirs
                (re-seq #"([RL]|\d+)")
                (mapv (fn [[_ m]] (or (parse-long m) m))))}))

(def example (parse "input/day22.example"))
(def input (parse "input/day22.input"))

(defn simplify-extent [extent]
  (->> extent
       (partition-by second)
       (map (fn [group] [[(ffirst group) (first (last group))] (second (first group))]))
       (into {})))

(defn extents [{:keys [grid]}]
  (let [max-y (apply max (map second (keys grid)))
        max-x (apply max (map first (keys grid)))]
    [(simplify-extent
      (for [i (range 0 (inc max-x))
            :let [column (keep (fn [j] (when (grid [i j]) [i j])) (range 0 (inc max-y)))]]
        [i [(second (first column)) (second (last column))]]))
     (simplify-extent
      (for [j (range 0 (inc max-y))
            :let [row (keep (fn [i] (when (grid [i j]) [i j])) (range 0 (inc max-x)))]]
        [j [(ffirst row) (first (last row))]]))]))

(extents example)
(extents input)

(def cw-dir {[1 0] [0 1] [0 1] [-1 0] [-1 0] [0 -1] [0 -1] [1 0]})
(def ccw-dir (set/map-invert cw-dir))
(defn v+ [a b] (mapv + a b))

(defn translate [grid extent pos facing]
  (let [pos' (v+ pos facing)
        [x y] pos
        [y0 y1] (some (fn [[[x0 x1] y-range]] (when (<= x0 x x1) y-range)) (first extent))
        [x0 x1] (some (fn [[[y0 y1] x-range]] (when (<= y0 y y1) x-range)) (second extent))]
    (if (grid pos')
      pos'
      (do
        (println "t:" pos pos' [x0 x1] [y0 y1])
        (case facing
          [1 0] [x0 y]
          [0 1] [x y0]
          [-1 0] [x1 y]
          [0 -1] [x y1])))))

(comment
  (translate (:grid example) (extents example) [10 6] [1 0])
  (translate (:grid example) (extents example) [11 6] [1 0])

  (translate (:grid example) (extents example) [5 4] [0 1])
  (translate (:grid example) (extents example) [5 4] [0 -1]))

(defn move [grid extent pos facing steps]
  (loop [pos pos n steps]
    (let [pos' (translate grid extent pos facing)
          loc (get grid pos')]
      (cond (zero? n)
            pos
            (= loc \#)
            pos
            (= loc \.)
            (recur pos' (dec n))
            :else
            [pos pos' loc]))))

(defn follow [{:keys [grid path start] :as input}]
  (let [extent (extents input)]
    (reductions
     (fn [[pos facing] op]
       (println pos facing op)
       (cond (= "R" op)
             [pos (cw-dir facing)]
             (= "L" op)
             [pos (ccw-dir facing)]
             (number? op)
             [(move grid extent pos facing op) facing]))
     [start [1 0]]
     path)))

(defn star1 [file]
  (let [input (parse file)
        [[x y] facing] (last (follow input))
        row (inc y)
        col (inc x)
        face (some (fn [[i dir]] (when (= facing dir) i))(map-indexed vector (keys cw-dir)))]
    [row col facing face (+ (* 1000 row) (* 4 col) face)]))

;;   1133
;;   1133
;;   22
;;   22
;; 4466
;; 4466
;; 55
;; 55

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day22")
 (fn [{:keys [result]}] result))
