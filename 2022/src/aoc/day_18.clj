(ns aoc.day-18
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]
   [clojure.set :as set]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 18 :name "example" :content "")
#_(aoc/day-input :day 18)

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (mapv parse-long (str/split line #","))))

(def example (parse "input/day18.example"))
(def input (parse "input/day18.input"))

#_(* 21 21 21)

(defn v+ [a b] (mapv + a b))

(defn faces [coord]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (= (+ (Math/abs dx) (Math/abs dy) (Math/abs dz)) 1)]
    (v+ coord [dx dy dz])))

#_(faces [0 0 0])
#_(faces (first example))

(defn exposed-faces [index coord]
  (->> coord
       faces
       (remove (fn [face] (contains? index face)))))

#_(faces [1 1 1])
#_(exposed-faces (set [[1 1 1] [2 1 1]]) [1 1 1])

(defn star1 [file]
  (let [cubes (parse file)
        index (set cubes)]
    (reduce + (map (fn [c] (count (exposed-faces index c))) cubes))))

#_(star1 "input/day18.example")

(defn min-max [xs]
  [(apply min xs) (apply max xs)])

(defn ranges [cubes]
  (let [[x0 x1] (min-max (map first cubes))
        [y0 y1] (min-max (map second cubes))
        [z0 z1] (min-max (map #(nth % 2) cubes))]
    (fn [[x y z]]
      (and (<= x0 x x1)
           (<= y0 y y1)
           (<= z0 z z1)))))

(defn flood [index in-range? interior exterior coord]
  (loop [queue [coord] seen #{coord}]
    (if (empty? queue)
      [true (set/union interior seen) exterior]
      (let [[node & remaining] queue
            exposed (remove (fn [c] (contains? seen c)) (exposed-faces index node))]
        (cond (contains? interior node)
              [true (set/union interior seen) exterior]
              (empty? exposed)
              (recur remaining (conj seen node))
              (every? in-range? exposed)
              (recur (into remaining exposed) (conj seen node))
              :else
              [false interior (set/union seen exterior (set exposed))])))))

#_(flood #{} (fn [[x y z]] (and (<= 0 x 2) (<= 0 y 2) (<= 0 z 2))) #{} #{} [1 1 1])

(defn filter-flood [index in-range? coords]
  (loop [interior #{} exterior #{} coords coords]
    (if (empty? coords)
      [interior exterior]
      (let [[coord & remaining] coords
            [_r interior' exterior'] (flood index in-range? interior exterior coord)]
        ;; (println coord (if _r "inside" "outside"))
        (recur interior' exterior' remaining)))))

(defn star2 [file]
  (let [cubes (parse file)
        index (set cubes)
        in-range? (ranges cubes)
        maybe-interior (->> cubes
                            (mapcat (fn [c] (exposed-faces index c)))
                            distinct
                            (filter in-range?)
                            (filter (fn [c] (every? in-range? (faces c)))))
        [interior _] (filter-flood index in-range? maybe-interior)
        with-interior (set/union (set (filter interior maybe-interior)) index)]
    (reduce + (map (fn [c] (count (exposed-faces with-interior c))) cubes))))

#_(star2 "input/day18.example")

;; 370 is too low
#_(star2 "input/day18.input")

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day18")
 (fn [{:keys [result]}] result))
