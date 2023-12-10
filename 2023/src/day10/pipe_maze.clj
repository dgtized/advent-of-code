(ns day10.pipe-maze
  (:require [clojure.string :as str]))

(def input (slurp "src/day10/input"))
(def example (slurp "src/day10/example"))
(def example2 (slurp "src/day10/example2"))
(def example3 (slurp "src/day10/example3"))

(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn parse [in]
  (->grid (str/split-lines in)))

(defn v+ [a b] (map + a b))

(def north [0 -1])
(def south [0 1])
(def east [1 0])
(def west [-1 0])
(def dirs
  {\| [north south]
   \- [east west]
   \L [north east]
   \J [north west]
   \7 [south west]
   \F [south east]
   \. []
   \S [north south east west]})

(defn neighbors [grid pos]
  (->> pos
       (get grid)
       (get dirs)
       (map (partial v+ pos))
       (filter grid)))

(assert (= [[3 2] [2 3]]
           (neighbors (parse example) [3 3])))

(defn init-neighbors [grid start]
  (filter (fn [p] (contains? (set (neighbors grid p)) start))
          (neighbors grid start)))

(defn find-start [grid]
  (some (fn [[p v]] (when (= v \S) p)) grid))

(let [grid (parse input)]
  (init-neighbors grid (find-start grid)))

(defn flood [grid start]
  (loop [to-visit (init-neighbors grid start) visited {start 0} d 1]
    (if (empty? to-visit)
      visited
      (recur (remove visited (mapcat (partial neighbors grid) to-visit))
             (reduce (fn [vis p] (update vis p (fnil min (inc d)) d)) visited to-visit)
             (inc d)))))

(defn part1 [grid]
  (apply max (vals (flood grid (find-start grid)))))

(assert (= 4 (part1 (parse example))))
(assert (= 4 (part1 (parse example2))))
(assert (= 8 (part1 (parse example3))))
(assert (= 6640 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
