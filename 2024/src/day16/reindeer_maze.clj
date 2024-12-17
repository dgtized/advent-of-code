(ns day16.reindeer-maze
  (:require
   [aoc.grid :as ag]
   [aoc.vector :as v]
   [aoc.graph :as graph]
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "src/day16/input"))
(def example (slurp "src/day16/example"))
(def example2 (slurp "src/day16/example2"))

(defn rotate-right [[x y]] [y (- x)])
(defn rotate-left [[x y]] [(- y) x])

(defn successors [grid [pos dir _cost]]
  (keep (fn [s]
          (when (contains? #{\. \S \E} (get grid (first s)))
            s))
        [[pos (rotate-left dir) 1000]
         [pos (rotate-right dir) 1000]
         [(v/v+ pos dir) dir 1]]))

(defn find-start [grid]
  (some (fn [[pos v]] (when (= v \S) pos)) grid))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn path-cost [states]
  (apply + (map last states)))

(defn search-path [grid]
  (time
   (graph/a*-search {:successors (fn [s] (successors grid s))
                     :cost (fn [[_ _ cost] _] cost)
                     :sources [[(find-start grid) [1 0] 0]]
                     :goal? (fn [s] (= (get grid (first s)) \E))} )))

(assert (= 7036 (path-cost (search-path (parse example)))))
(assert (= 106512 (path-cost (search-path (parse input)))))

;; calculate the best path, and then for each position in that path, remove it,
;; and accept any new path with the same cost as the best path.
(defn all-best-paths [grid]
  (let [best-path (search-path grid)
        best-cost (path-cost best-path)
        path-set (set (map first best-path))]
    (reduce set/union
            path-set
            (for [pos path-set
                  :let [path (search-path (assoc grid pos \#))]
                  :when (= (path-cost path) best-cost)]
              (set (map first path))))))

(assert (= 44 (count (all-best-paths (parse example)))))
(assert (= 64 (count (all-best-paths (parse example2)))))
;; slow, takes ~350ms per node in the path, so 350ms*619 nodes = 216s
(println (count (all-best-paths (parse input))))
;; (assert (= 563 (count (all-best-paths (parse input)))))
