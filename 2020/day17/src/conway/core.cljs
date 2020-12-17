(ns conway.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as mw]
            [clojure.string :as str]))

;; Usage:
;; npm install # overrides p5.js version to latest
;; clojure -m figwheel.main # browser should open with 3d display, n goes to next step

(enable-console-print!)

(def input ".#######
#######.
###.###.
#....###
.#..##..
#.#.###.
###..###
.#.#.##.")

(def example ".#.
..#
###")

(defn parse [s]
  (let [lines (str/split-lines s)
        size (count lines)]
    (for [y (range 0 size)
          x (range 0 size)
          :let [v (subs (nth lines y) x (+ x 1))]
          :when (= v "#")]
      [x y 0 0])))

(comment (parse example)
         (parse input))

(defn neighbor-coords [[x y z w]]
  (for [dx (range -1 2)
        dy (range -1 2)
        dz (range -1 2)
        dw (range -1 2)
        :when (not= dx dy dz dw 0)]
    [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))

(defn index-points [points]
  (into {} (for [p points] [p p])))

(defn neighbors-of [point points-lookup]
  (keep identity
        (for [neighbor-point (neighbor-coords point)]
          (get points-lookup neighbor-point nil))))

(defn cube-bounds [points]
  (let [positions points]
    (for [idx (range 0 4)]
      [(dec ((apply min-key #(nth % idx) positions) idx))
       (inc (inc ((apply max-key #(nth % idx) positions) idx)))])))

(comment (count (neighbor-coords [0 0 0 0]))
         (neighbors-of [1 1 0 0] (index-points (parse example)))
         (cube-bounds (parse example)))

(defn next-state [points]
  (let [points-index (index-points points)
        bounds (cube-bounds points)]
    (keep identity (for [x (apply range (nth bounds 0))
                         y (apply range (nth bounds 1))
                         z (apply range (nth bounds 2))
                         w (apply range (nth bounds 3))
                         :let [position [x y z w]
                               current-point (get points-index position)
                               neighbors (neighbors-of position points-index)]]
                     (cond (and current-point (#{2 3} (count neighbors)))
                           current-point
                           (and (not current-point) (= 3 (count neighbors)))
                           position)))))

(comment ;; solution
  ;; note this is broken up because of timeout problems
  (def e5 (nth (iterate next-state (parse example)) 5))
  (count e5)
  (= 848 (count (next-state e5)))

  ;; problems with timeout, works when clicking n 6 times for displayed example
  (def i5 (nth (iterate next-state (parse input)) 5))
  (= 2812 (count i5))
  ;; something unhappy about lazy evaluation, blows stack
  (= 2296 (count (next-state i5))))

(defn setup []
  {:points (parse input)})

(defn keypress [state event]
  (cond (= :n (:key event))
        (let [state (update-in state [:points] next-state)]
          (println "Points: " (count (:points state)))
          state)
        :else state))

(defn draw [{:keys [points] :as state}]
  (q/background "white")
  (q/stroke 0)
  (q/no-fill)
  (q/stroke-weight 10)
  (q/scale 20)
  (q/translate 10 10 -20)
  (doseq [p points]
    (apply q/point (butlast p))))

(q/defsketch conway
  :host "quil-host"
  :renderer :p3d
  :size [500 500]
  :setup setup
  :draw draw
  :key-pressed keypress
  :middleware [mw/fun-mode mw/navigation-3d]
  :navigation-3d {:position [200 200 50]})

