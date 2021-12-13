(ns transparent-origami
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [filename]
  (let [[coords folds] (str/split (slurp filename) #"\n\n")]
    [(reduce (fn [grid line]
               (conj grid (mapv parse-long (str/split line #","))))
             #{} (str/split-lines coords))
     (map (partial drop 1) (re-seq #"([xy])=(\d+)" folds))]))

(defn fold-y [y-split coords]
  (let [[above below] (split-with (fn [[_ y]] (< y y-split))
                                  (sort-by second coords))]
    (set/union above
               (set (for [[x y] below]
                      [x (- y-split (- y y-split))])))))

(defn fold-x [x-split coords]
  (let [[left right] (split-with (fn [[x _]] (< x x-split))
                                 (sort-by first coords))]
    (set/union
     (set (for [[x y] left]
            [(- x-split 1 x) y]))
     (set (for [[x y] right]
            [(- x 1 x-split) y])))))

(assert (= #{[4 0]} (fold-x 5 [[0 0]])))
(assert (= #{[3 0]} (fold-x 5 [[1 0]])))
(assert (= #{[0 0]} (fold-x 5 [[4 0]])))
(assert (= #{[0 0]} (fold-x 5 [[6 0]])))
(assert (= #{[1 0]} (fold-x 5 [[7 0]])))
(assert (= #{[3 0]} (fold-x 5 [[9 0]])))

(assert (= #{[0 0] [1 0] [2 0] [3 0] [4 0]}
           (fold-x 5 [[0 0] [2 0] [3 0] [6 0] [9 0]])))

(defn fold [coords [axis offset]]
  (if (= "x" axis)
    (fold-x (parse-long offset) coords)
    (fold-y (parse-long offset) coords)))

(defn part1 [[coords folds]]
  (count (fold coords (first folds))))

(assert (= 17 (part1 (parse "example"))))
(assert (= 693 (part1 (parse "input"))))
