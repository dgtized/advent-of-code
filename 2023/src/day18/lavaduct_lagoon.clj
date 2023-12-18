(ns day18.lavaduct-lagoon
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day18/input"))
(def example (slurp "src/day18/example"))

(def dirs {"R" [1 0]
           "L" [-1 0]
           "D" [0 1]
           "U" [0 -1]})

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[dir dist color] (str/split line #" ")]
      [(get dirs dir) (parse-long dist) (str/replace color #"[()]" "")])))

(defn v+ [a b] (mapv + a b))
(defn v* [[x y] c] [(* x c) (* y c)])

(defn outline [steps]
  (reduce (fn [[grid pos] [dir dist color]]
            [(reduce (fn [g c] (assoc g (v+ pos (v* dir c)) color))
                     grid (range 0 dist))
             (v+ pos (v* dir dist))])
          [{} [0 0]]
          steps))

(defn outline-pts [steps]
  (reductions (fn [pos [dir dist]]
                (v+ pos (v* dir dist)))
              [0 0]
              steps))

(defn dims [grid]
  (let [xs (map first (keys grid))
        ys (map second (keys grid))]
    [[(- (apply min xs) 1) (+ (apply max xs) 2)]
     [(- (apply min ys) 1) (+ (apply max ys) 2)]]))

(defn neighbors [[[x0 x1] [y0 y1]] pos]
  (for [d directions
        :let [[x y :as n] (v+ pos d)]
        :when (and (<= x0 x x1) (<= y0 y y1))]
    n))

(defn flood-outside [grid start]
  (let [dims (dims grid)]
    (loop [outside #{start}
           queue (conj clojure.lang.PersistentQueue/EMPTY start)
           grid grid]
      (if (empty? queue)
        grid
        (let [pos (peek queue)
              val (get grid pos)]
          (cond (nil? val)
                (let [neighbors (neighbors dims pos)]
                  (recur (set/union outside (set neighbors))
                         (into (pop queue) neighbors)
                         (assoc grid pos "O")))
                :else
                (recur outside
                       (pop queue)
                       grid)))))))

(defn region [grid]
  (let [[[x0 x1] [y0 y1]] (dims grid)]
    (for [j (range (inc y0) (dec y1))]
      (apply str (mapv (fn [i] (let [v (get grid [i j])]
                                (cond (= v "O")
                                      v
                                      (some? v)
                                      "#"
                                      :else
                                      ".")))
                       (range (inc x0) (dec x1)))))))

(defn part1 [in]
  (let [[grid _pos] (outline in)
        [[x0 _] [y0 _]] (dims grid)
        field (flood-outside grid [x0 y0])
        r (region field)
        f (frequencies (apply str r))]
    (spit "src/day18/region" (str/join "\n" r))
    (+ (get f \#) (get f \.))))

;; (assert (= 62 (part1 (parse example))))
;; (assert (= 50603 (part1 (parse input))))

(defn area [pts]
  (/ (apply + (map (fn [[x0 y0] [x1 y1]]
                     (* (+ x1 x0) (- y1 y0)))
                   (butlast pts) (rest pts)))
     2))

(defn manhattan [[x0 y0] [x1 y1]]
  (+ (Math/abs (- x1 x0)) (Math/abs (- y1 y0))))

(defn perimeter [pts]
  (apply +
         (map (fn [a b] (manhattan a b))
              (butlast pts) (rest pts))))

(defn p1 [in]
  (let [pts (outline-pts in)]
    (+ (area pts) (inc (/ (perimeter pts) 2)))))

(assert (= 62 (p1 (parse example))))
(assert (= 50603 (p1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
