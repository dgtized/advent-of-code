(ns lobby-layout
  (:require [clojure.string :as str]))

(defn parse [filename]
  (->> filename
       slurp
       str/split-lines
       (map (partial re-seq #"e|se|sw|w|nw|ne"))))

;; Using Axial coordinates from https://www.redblobgames.com/grids/hexagons/
(defn follow [direction]
  (case direction
    "e"  [1 0]
    "se" [0 1]
    "sw" [-1 1]
    "w"  [-1 0]
    "nw" [0 -1]
    "ne" [1 -1]))

(defn tile-coord [directions]
  (reduce vector-add [0 0] (map follow directions)))

(defn vector-add [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn update-grid [grid positions]
  (loop [grid grid
         positions positions]
    (if (empty? positions)
      grid
      (recur (update grid (first positions) (fnil not true))
             (rest positions)))))

(defn first-star [positions]
  (-> (update-grid {} positions)
      vals
      frequencies
      (get false)))

(comment
  (first-star (map tile-coord (parse "example")))
  (first-star (map tile-coord (parse "input"))))
