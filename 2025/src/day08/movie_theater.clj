(ns day08.movie-theater
  (:require
   [aoc.combinatorics :as ac]
   [clojure.string :as str])
  (:import
   (java.awt Color)
   (java.awt.image BufferedImage)
   (java.io File)
   (javax.imageio ImageIO)))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (mapv parse-long (re-seq #"\d+" line))))

(defn area [[[x0 y0] [x1 y1]]]
  (* (inc (abs (- x1 x0))) (inc (abs (- y1 y0)))))

(comment (area [[2 5] [9 7]])
         (area [[9 7] [2 5]]))

(defn part1 [in]
  (mapv area (ac/all-pairs in)))

(assert (= 50 (apply max (part1 (parse example)))))
(assert (= 4781546175 (apply max (part1 (parse input)))))

(defn space-range [points]
  (let [xs (map first points)
        ys (map second points)]
    [[(apply min xs)
      (apply max xs)]
     [(apply min ys)
      (apply max ys)]]))

(space-range (parse input))

(defn render [points scale size]
  (let [
        rv (fn [[x y]]
             [(int (* size (/ x scale))) (int (* size (/ y scale)))])
        image (BufferedImage. size size BufferedImage/TYPE_INT_ARGB)
        gr (.getGraphics image)]
    (doto gr (.setColor Color/black)
          (.fillRect 0 0 size size))
    ;; (doto gr (.setColor Color/green))
    ;; (doseq [[a b] (ac/all-pairs points)]
    ;;   (let [[x0 y0] (rv a)
    ;;         [x1 y1] (rv b)]
    ;;     (doto gr (.fillRect x0 y0 x1 y1))))
    (doto gr (.setColor Color/red))
    (doseq [[a b] (mapv vector points (rest points))]
      (let [[x0 y0] (rv a)
            [x1 y1] (rv b)]
        (doto gr (.drawLine x0 y0 x1 y1))))
    (ImageIO/write image "png" (File. "src/day08/image.png"))))

(render (parse input) 100000.0 1000)

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
