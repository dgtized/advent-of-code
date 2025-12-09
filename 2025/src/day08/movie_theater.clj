(ns day08.movie-theater
  (:require
   [aoc.combinatorics :as ac]
   [clojure.math :as math]
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
(space-range (parse example))

(defn render [points scale size desc rects]
  (let [rv (fn [[x y]]
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
    (doto gr (.setColor Color/blue))
    (let [[x0 y0] (rv (last points))
          [x1 y1] (rv (first points))]
      (doto gr (.drawLine x0 y0 x1 y1)))
    (doto gr (.setColor Color/green))
    (doseq [[a b] rects]
      (let [[x0 y0] (rv a)
            [x1 y1] (rv b)]
        (doto gr (.drawRect (min x0 x1) (min y0 y1) (- (max x0 x1) (min x0 x1)) (- (max y0 y1) (min y0 y1))))))
    (ImageIO/write image "png" (File. (str "src/day08/" desc ".png")))))

(render (parse input) 100000.0 1600 "input" [])
(render (parse example) 12.0 1600 "example" [])

(defn distance [[x0 y0] [x1 y1]]
  (math/sqrt (+ (math/pow (- x1 x0) 2) (math/pow (- y1 y0) 2))))

;; find inset bounds
(let [points (parse input)]
  (for [[a b] (mapv vector points (rest points))
        :let [d (distance a b)]
        :when (> d 1600)]
    [a b d]))

(comment ;; results
  ([[1840 50058] [94876 50058] 93036.0]
   [[94876 48734] [2512 48734] 92364.0]))

(count (parse input)) ;; 496
(count (filter (fn [[x _]] (< x 94876)) (parse input))) ;; 436
(count (filter (fn [[x y]] (and (< x 94876) (< y 48734))) (parse input))) ;; 217
(count (filter (fn [[x y]] (and (< x 94876) (> y 50058))) (parse input))) ;; 217
(* 217 2) ;; 434

(defn point-in-rect? [[[x0 y0] [x1 y1]] [x y]]
  (and (<= (min x0 x1) x (max x0 x1))
       (<= (min y0 y1) y (max y0 y1))))

(comment (point-in-rect? [[0 0] [10 10]] [0 0])
         (point-in-rect? [[0 0] [10 10]] [10 0])
         (point-in-rect? [[0 0] [10 10]] [11 0])
         (point-in-rect? [[0 0] [10 10]] [0 11])
         (point-in-rect? [[0 0] [10 10]] [11 11]))

(defn legal-rectangles [p1 dir points]
  (let [[x1 y1] p1
        pts (filter (fn [[x y]] (and (< x x1) (dir y y1))) points)]
    (sort-by second
             (for [pt pts
                   :let [rect [pt p1]]
                   :when (not-any? (fn [p] (point-in-rect? rect p)) (remove (fn [p] (= p pt)) pts))]
               [rect (area rect)]))))

(legal-rectangles [94876 48734] < (parse input))
;; 1275903376
(legal-rectangles [94876 50058] > (parse input))
;; 1565730054 ;; too low

(render (parse input) 100000 1600 "solutions"
        [[[4974 34543] [94876 48734]]
         [[6011 67676] [94876 50058]]])

(defn part2 [in]
  )

;; (assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
