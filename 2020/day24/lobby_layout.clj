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

(defn vector-add [[x0 y0] [x1 y1]]
  [(+ x0 x1) (+ y0 y1)])

(defn tile-coord [directions]
  (reduce vector-add [0 0] (map follow directions)))

(defn update-grid [grid positions]
  (loop [grid grid
         positions positions]
    (if (empty? positions)
      grid
      (recur (update grid (first positions) (fnil not true))
             (rest positions)))))

(defn calculate-grid [filename]
  (update-grid {} (map tile-coord (parse filename))))

(defn first-star [grid]
  (-> grid
      vals
      frequencies
      (get false)))

(defn neighboring-coordinates [position]
  (map (comp (partial vector-add position) follow)
       ["e" "se" "sw" "w" "nw" "ne"]))

(defn assoc-if-missing [map key value]
  (if (contains? map key)
    map
    (assoc map key value)))

(defn add-missing-neighbors [grid]
  (->> (keys grid)
       (mapcat neighboring-coordinates)
       distinct
       (reduce (fn [grid coord] (assoc-if-missing grid coord true))
               grid)))

(defn count-neighbors [grid coord]
  (get (frequencies (map (fn [p] (get grid p true))
                         (neighboring-coordinates coord)))
       false 0))

(defn grid-step [grid]
  ;; (println (first-star grid))
  (into {}
        (for [[coord value] (add-missing-neighbors grid)
              :let [black (count-neighbors grid coord)]]
          (cond (and (= value false) (or (= black 0) (> black 2)))
                [coord true]
                (and (= value true) (= black 2))
                [coord false]
                :else
                [coord value]))))

(defn second-star [grid n]
  (first-star (last (take (+ 1 n) (iterate grid-step grid)))))

(comment
  (first-star (calculate-grid "example"))
  (first-star (calculate-grid "input"))

  (neighboring-coordinates [0 0])
  (add-missing-neighbors {[0 0] false [0 1] false})

  (= 1 (count-neighbors {[0 0] false [1 0] false} [0 0]))

  (add-missing-neighbors (calculate-grid "example"))
  (grid-step (calculate-grid "example"))

  (= 2208 (second-star (calculate-grid "example") 100))
  (= 3737 (second-star (calculate-grid "input") 100))
  )
