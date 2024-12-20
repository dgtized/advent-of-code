(ns day20.race-condition
  (:require
   [aoc.combinatorics :as ac]
   [aoc.graph :as graph]
   [aoc.grid :as ag]
   [aoc.vector :as v]
   [clojure.string :as str]))

(def input (slurp "src/day20/input"))
(def example (slurp "src/day20/example"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn successors [grid pos]
  (keep (fn [s]
          (when (contains? #{\. \S \E} (get grid pos))
            s))
        (map (fn [dir] (v/v+ pos dir))
             v/cardinal)))

(defn search [grid]
  (graph/a*-search
   {:successors (fn [pos] (successors grid pos))
    :sources [(ag/some-value grid \S)]
    :goal? (fn [pos] (= (get grid pos) \E))}))

(defn cheats [grid limit]
  (let [path (search grid)
        path-idx (into {} (map vector path (range (count path))))]
    (mapcat (fn [pos]
              (for [dir v/cardinal
                    :let [c1 (v/v+ pos dir)
                          c2 (v/v+ c1 dir)]
                    :when (and (= \# (get grid c1))
                               (> (get path-idx c2 0) (get path-idx pos))
                               (>= (- (get path-idx c2) (get path-idx pos) 2) limit))]
                [[c1 c2] (- (get path-idx c2 0) (get path-idx pos) 2)]))
            path)))

(assert (= (+ 14 14 2 4 2 3 1 1 1 1 1) (count (cheats (parse example) 1))))
(assert (= 1351 (count (cheats (parse input) 100))))

(defn manhattan [[x0 y0] [x1 y1]]
  (+ (abs (- x1 x0))
     (abs (- y1 y0))))

(defn part2 [grid limit]
  (let [path (search grid)
        path-idx (into {} (map vector path (range (count path))))]
    (for [[p1 p2] (ac/all-pairs path)
          :let [cut-cost (manhattan p1 p2)
                path-cost (- (get path-idx p2) (get path-idx p1))
                savings (- path-cost cut-cost)]
          :when (and (<= cut-cost 20)
                     (< (get path-idx p1) (get path-idx p2))
                     (>= savings limit))]
      savings)))

;; (+ 32 31 29 39 25 23 20 19 12 14 12 22 4 3) => 285
(assert (= (sort-by first (frequencies (part2 (parse example) 50)))))
(assert (= 966130 (count (part2 (parse input) 100))))
