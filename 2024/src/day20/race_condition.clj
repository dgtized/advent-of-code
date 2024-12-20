(ns day20.race-condition
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]
            [aoc.graph :as graph]))

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
    (println path-idx)
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

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
