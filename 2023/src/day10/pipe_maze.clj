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

(defn grid->line
  [grid path-grid]
  (let [w (apply max (map first (keys grid)))
        h (apply max (map second (keys grid)))]
    (->> (for [j (range (inc h))
               i (range (inc w))]
           (if (get path-grid [i j]) (get grid [i j]) \.))
         (partition (inc w))
         (mapv vec))))

(defn odd-even-row [grid row j]
  (reduce (fn [[isecs  inside] [p i]]
            (case p \.
                  (if (odd? isecs)
                    [isecs (conj inside [i j])]
                    [isecs inside])
                  \-
                  [isecs inside]
                  \|
                  [(inc isecs) inside]
                  \F
                  [(inc isecs) inside]
                  \L
                  [(inc isecs) inside]
                  \7
                  [(inc isecs) inside]
                  \J
                  [(inc isecs) inside]
                  \S
                  (if (and (#{\F \L \-} (get grid [(dec i) j] "."))
                           (#{\J \7 \-} (get grid [(inc i) j] ".")))
                    [isecs inside]
                    [(inc isecs) inside])))
          [0 []]
          (map vector row (range (count row)))))

(defn space [grid]
  (let [path-grid (flood grid (find-start grid))
        cgrid (grid->line grid path-grid)]
    [(mapv #(apply str %) cgrid)
     (map (partial odd-even-row grid) cgrid (range (count grid)))]))

(defn part2 [[g v]]
  (count (mapcat last v)))

;; (count (filter #(= \. %) (apply concat (part2 (parse input))))) => 6320 to check

(assert (= 4 (part2 (space (parse (slurp "src/day10/contained1"))))))
(assert (= 8 (part2 (space (parse (slurp "src/day10/contained2"))))))
(assert (= 10 (part2 (space (parse (slurp "src/day10/contained3"))))))
(assert (= (part2 (space (parse input)))))
