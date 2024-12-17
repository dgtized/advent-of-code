(ns day06.guard-gallivant
  (:require [aoc.grid :as ag]
            [clojure.string :as str]
            [aoc.vector :as v]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn clockwise [face]
  (->> v/cardinal
       cycle
       (drop-while (fn [dir] (not= dir face)))
       (drop 1)
       first))

(comment (clockwise [1 0])
         (clockwise [0 1])
         (clockwise [-1 0])
         (clockwise [0 -1]))

(defn find-guard [grid]
  (some (fn [[cell v]] (when (= v \^) cell)) grid))

(defn step [[grid pos dir steps]]
  (let [pos' (v/v+ pos dir)]
    (case (get grid pos')
      \# [grid pos (clockwise dir) (conj steps [pos dir])]
      \. [(assoc grid pos \X) pos' dir (conj steps [pos dir])]
      \X [(assoc grid pos \X) pos' dir (conj steps [pos dir])]
      nil [(assoc grid pos \X) pos' dir steps])))

(defn walk [grid]
  (let [start (find-guard grid)]
    (some (fn [[grid pos _]] (when-not (get grid pos) grid))
          (iterate step [grid start [0 -1] #{}]))))

(defn part1 [in]
  (count (filter #(= % \X) (vals (walk in)))))

(assert (= 41 (part1 (parse example))))
(assert (= 5239 (part1 (parse input))))

(defn loop? [start grid]
  (some (fn [[grid pos dir steps]]
          (cond (and (seq steps) (contains? steps [pos dir]))
                [:loop (count steps)]
                (not (get grid pos))
                [:outside (count steps)]))
        (iterate step [grid start [0 -1] #{}])))

(defn part2 [grid]
  (let [start (find-guard grid)]
    (filter #(= (first %) :loop)
            (let [pathed-grid (walk grid)]
              (for [[cell v] pathed-grid
                    :when (and (not= start cell) (= v \X))]
                (loop? start (assoc grid cell \#)))))))

(assert (= 6 (count (part2 (parse example)))))
;; initial slow attempt: 125605.81 msecs
;; only check positions on path: 28152.15 msecs
(time (assert (= 1753 (count (part2 (parse input))))))
