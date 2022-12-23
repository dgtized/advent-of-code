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

(defn translate [extent grid pos facing]
  (let [pos' (v+ pos facing)
        [x y] pos
        [y0 y1] (some (fn [[[x0 x1] y-range]] (when (<= x0 x x1) y-range)) (first extent))
        [x0 x1] (some (fn [[[y0 y1] x-range]] (when (<= y0 y y1) x-range)) (second extent))]
    [(if (grid pos')
       pos'
       (case facing
         [1 0] [x0 y]
         [0 1] [x y0]
         [-1 0] [x1 y]
         [0 -1] [x y1]))
     facing]))

(comment
  (translate (extents example) (:grid example) [10 6] [1 0])
  (translate (extents example) (:grid example) [11 6] [1 0])

  (translate (extents example) (:grid example) [5 4] [0 1])
  (translate (extents example) (:grid example) [5 4] [0 -1]))

(defn move [translate grid pos facing steps]
  (loop [pos pos facing facing n steps]
    (let [[pos' facing'] (translate grid pos facing)
          loc (get grid pos')]
      (cond (zero? n)
            [pos facing]
            (= loc \#)
            [pos facing]
            (= loc \.)
            (recur pos' facing' (dec n))
            :else
            [pos pos' loc]))))

(defn follow [{:keys [grid path start]} translate]
  (reductions
   (fn [[pos facing] op]
     ;; (println pos facing op)
     (cond (= "R" op)
           [pos (cw-dir facing)]
           (= "L" op)
           [pos (ccw-dir facing)]
           (number? op)
           (move translate grid pos facing op)
           :else
           (reduced [pos facing op])))
   [start [1 0]]
   path))

(defn star1 [file]
  (let [input (parse file)
        extent (extents input)
        translate (partial translate extent)
        [[x y] facing] (last (follow input translate))
        row (inc y)
        col (inc x)
        face (some (fn [[i dir]] (when (= facing dir) i))(map-indexed vector (keys cw-dir)))]
    [row col facing face (+ (* 1000 row) (* 4 col) face)]))

;;     11
;;     11
;; 223344
;; 223344
;;     5566
;;     5566

;; 1U -> 2U facing D
;; 1L -> 3U facing D
;; 1R -> 6R facing L
;; 2U -> 1U facing D
;; 2L -> 6D facing U
;; 2D -> 5D facing U
;; 3U -> 1L facing R
;; 3D -> 5L facing R
;; 4R -> 6U facing D
;; 5L -> 3D facing U
;; 5D -> 2D facing U
;; 6U -> 4R facing L
;; 6R -> 1R facing L
;; 6D -> 2L facing R

;;   1122
;;   1122
;;   33
;;   33
;; 4455
;; 4455
;; 66
;; 66

;; 1U -> 6L facing R
;; 1L -> 4L facing R
;; 2U -> 6D facing U
;; 2R -> 5R facing L
;; 2D -> 3R facing L
;; 3L -> 4U facing D
;; 3R -> 2D facing U
;; 4U -> 3L facing R
;; 4L -> 1L facing R
;; 5R -> 2D facing U
;; 5D -> 6R facing L
;; 6L -> 1U facing D
;; 6D -> 2U facing D
;; 6R -> 5D facing U

(def example-faces
  [[[8 11] [0 3]]
   [[0 3] [4 7]]
   [[4 7] [4 7]]
   [[8 11] [4 7]]
   [[8 11] [8 11]]
   [[12 15] [8 11]]])

(def input-faces
  [[[50 99] [0 49]]
   [[100 149] [0 49]]
   [[50 99] [50 99]]
   [[0 49] [100 149]]
   [[50 99] [100 149]]
   [[0 49] [150 199]]])


(defn face [grid [x y]]
  (->> (if (< (count grid) 100) example-faces input-faces)
       (map-indexed vector)
       (some (fn [[face [[x0 x1] [y0 y1]]]]
               (when (and (<= x0 x x1) (<= y0 y y1)) face)))))

(def example-rots
  {[1 :U] [2 :U]
   [1 :L] [3 :U]
   [1 :R] [6 :R]
   [2 :U] [1 :U]
   [2 :L] [6 :D]
   [2 :D] [5 :D]
   [3 :U] [1 :L]
   [3 :D] [5 :L]
   [4 :R] [6 :U]
   [5 :L] [3 :D]
   [5 :D] [2 :D]
   [6 :U] [4 :R]
   [6 :R] [1 :R]
   [6 :D] [2 :L]})

(def input-rots
  {[1 :U] [6 :L]
   [1 :L] [4 :L]
   [2 :U] [6 :D]
   [2 :R] [5 :R]
   [2 :D] [3 :R]
   [3 :L] [4 :U]
   [3 :R] [2 :D]
   [4 :U] [3 :L]
   [4 :L] [1 :L]
   [5 :R] [2 :R]
   [5 :D] [6 :R]
   [6 :L] [1 :U]
   [6 :D] [2 :U]
   [6 :R] [5 :D]})

(comment (face (:grid example) [8 0])
         (face (:grid input) [50 0]))

(def dirs
  {[1 0] :R
   [0 1] :D
   [-1 0] :L
   [0 -1] :U})

(defn translate-rot [grid pos facing]
  (let [pos' (v+ pos facing)
        faces (if (< (count grid) 100) example-faces input-faces)
        rotations (if (< (count grid) 100) example-rots input-rots)]
    (if (grid pos')
      [pos' facing]
      (let [side (get dirs facing)
            face (face grid pos)
            [x y] pos
            [[x0 x1] [y0 y1]] (nth faces face)
            [face' side'] (get rotations [(inc face) side])
            [[x0' x1'] [y0' y1']] (nth faces (dec face'))]
        [(case [side side']
           [:L :L] [x0' (+ y0' (- y1 y))]
           [:L :R] [x1' (+ y0' (- y y0))]
           [:L :U] [(+ x0' (- y y0)) y0']
           [:L :D] [(+ x0' (- y1 1)) y1']
           [:D :L] [x0' (+ y0' (- x1 x))]
           [:D :R] [x1' (+ y0' (- x x0))]
           [:D :U] [(+ x0' (- x x0)) y0']
           [:D :D] [(+ x0' (- x1 x)) y1']
           [:U :L] [x0' (+ y0' (- x x0))]
           [:U :R] [x1' (+ y0' (- x1 x))]
           [:U :U] [(+ x0' (- x1 x)) y0']
           [:U :D] [(+ x0' (- x x0)) y1']
           [:R :L] [x0' (+ y0' (- y y0))]
           [:R :R] [x1' (+ y0' (- y1 y))]
           [:R :U] [(+ x0' (- y1 y)) y0']
           [:R :D] [(+ x0' (- y y0)) y1'])
         (mapv (fn [x] (- x)) (get (set/map-invert dirs) side'))]))))

(defn star2 [file]
  (let [input (parse file)
        [[x y] facing] (last (follow input translate-rot))
        row (inc y)
        col (inc x)
        face (->> cw-dir
                  keys
                  (map-indexed vector)
                  (some (fn [[i dir]] (when (= facing dir) i))))]
    [row col facing face (+ (* 1000 row) (* 4 col) face)]))

(defn show-grid [file]
  (let [input (parse file)
        path (follow input translate-rot)
        dir {[1 0] \> [0 1] \v [-1 0] \< [0 -1] \^}
        grid (reduce (fn [g [p d]] (assoc g p (get dir d)))
                     (:grid input) path)
        max-x (apply max (map first (keys grid)))
        max-y (apply max (map second (keys grid)))]
    (into [(str "    "
                (apply str
                       (for [i (range 0 (inc max-x))]
                         (mod i 10))))]
          (for [j (range 0 (inc max-y))]
            (format "%3d %s" j (apply str (map (fn [i] (get grid [i j] \space)) (range 0 (inc max-x)))))))))

#_(map vector (follow example translate-rot) (:path example))
#_(map vector (follow input translate-rot) (:path input))

(comment
  (= (translate-rot (:grid example) [11 5] [1 0]) [[14 8] [0 1]])
  (= (translate-rot (:grid example) [11 6] [1 0]) [[13 8] [0 1]])
  (= (translate-rot (:grid example) [11 1] [1 0]) [[15 10] [-1 0]])
  (= (translate-rot (:grid example) [11 2] [1 0]) [[15 9] [-1 0]])
  (= (translate-rot (:grid example) [8 0] [0 -1]) [[3 4] [0 1]])
  (= (translate-rot (:grid example) [10 11] [0 1]) [[1 7] [0 -1]])
  (= (translate-rot (:grid example) [6 4] [0 -1]) [[8 2] [1 0]])
  (= (translate-rot (:grid example) [7 4] [0 -1]) [[8 3] [1 0]])
  (= (translate-rot (:grid example) [8 3] [-1 0]) [[7 4] [0 1]])
  (= (translate-rot (:grid example) [8 2] [-1 0]) [[6 4] [0 1]]))

(star2 "input/day22.example")
(show-grid "input/day22.example")

(star2 "input/day22.input")
(show-grid "input/day22.input")

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day22")
 (fn [{:keys [result]}] result))
