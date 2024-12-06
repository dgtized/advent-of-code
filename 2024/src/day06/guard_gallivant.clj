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

(defn step [[grid pos dir steps]]
  (let [pos' (v/v+ pos dir)]
    (case (get grid pos')
      \# [grid pos (clockwise dir) (conj steps [pos dir])]
      \. [(assoc grid pos \X) pos' dir (conj steps [pos dir])]
      \X [(assoc grid pos \X) pos' dir (conj steps [pos dir])]
      nil [(assoc grid pos \X) pos' dir steps])))

(defn walk [grid]
  (let [start (some (fn [[cell v]] (when (= v \^) cell)) grid)]
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
  (let [start (some (fn [[cell v]] (when (= v \^) cell)) grid)]
    (count (filter #(= (first %) :loop)
                   (for [[cell v] grid
                         :when (= v \.)]
                     (loop? start (assoc grid cell \#)))))))

(assert (= 6 (part2 (parse example))))
;; slow 125605.808097 msecs
(time (assert (= 1753 (part2 (parse input)))))
