(ns day17.clumsy-crucible
  (:require
   [clojure.data.priority-map :as dpm]
   [clojure.string :as str]))

(def input (slurp "src/day17/input"))
(def example (slurp "src/day17/example"))

(defn parse [in]
  (into {}
        (for [[j line] (map-indexed vector (str/split-lines in))
              [i tile] (map-indexed vector line)]
          {[i j] (parse-long (str tile))})))

(defn path-cost [grid path]
  (apply + (map grid path)))

(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

(defn a*-search
  [{:keys [successors sources goal?
           cost heuristic] :or
    {cost (constantly 1)
     heuristic (constantly 1)}}]
  (loop [visited {}
         queue (reduce (fn [pq s] (assoc pq s [0 0 nil]))
                       (dpm/priority-map-keyfn first)
                       sources)]
    (when (seq queue)
      (let [[current [_ value prev]] (peek queue)
            visited' (assoc visited current prev)]
        (if (goal? current)
          (reverse (backtrack current visited'))
          (recur visited'
                 (reduce (fn [queue node]
                           (let [score (+ value (cost current node))]
                             (if (and (not (contains? visited' node))
                                      (or (not (contains? queue node))
                                          (< score (get-in queue [node 1]))))
                               (assoc queue node [(+ score (heuristic node)) score current])
                               queue)))
                         (pop queue)
                         (successors current))))))))

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))

(defn rotate-right [[x y]] [y (- x)])
(defn rotate-left [[x y]] [(- y) x])

(defn successors [grid [pos dir n]]
  (keep (fn [s]
          (when (and s (get grid (first s)))
            s))
        [(let [d' (rotate-left dir)]
           [(v+ pos d') d' 1])
         (let [d' (rotate-right dir)]
           [(v+ pos d') d' 1])
         (when (< n 3)
           [(v+ pos dir) dir (inc n)])]))

(defn successors-ultra [grid [pos dir n]]
  (keep (fn [s]
          (when (and s (get grid (first s)))
            s))
        [(when (> n 3)
           (let [d' (rotate-left dir)]
             [(v+ pos d') d' 1]))
         (when (> n 3)
           (let [d' (rotate-right dir)]
             [(v+ pos d') d' 1]))
         (when (< n 10)
           [(v+ pos dir) dir (inc n)])]))

(successors (parse example) [[0 0] [1 0] 0])
(successors (parse example) [[0 4] [1 0] 1])

(defn exit [grid]
  [(apply max (map first (keys grid)))
   (apply max (map second (keys grid)))])

(defn part1 [grid]
  (let [[x y] (exit grid)
        path (map first (a*-search
                         {:cost (fn [s _] (get grid (first s) 100))
                          :successors (fn [s] (successors grid s))
                          :sources [[[0 0] [0 1] 0]
                                    [[0 0] [1 0] 0]]
                          :goal? (fn [s] (= (first s) [x y]))}))]
    (let [pset (set path)]
      (doseq [j (range (inc y))]
        (println (apply str (for [i (range (inc x))]
                              (if (contains? pset [i j]) (get grid [i j]) "."))))))
    (println path)
    (- (path-cost grid path) (get grid [0 0]))))

(assert (= 102 (part1 (parse example))))
(assert (= 843 (part1 (parse input))))

(defn part2 [grid]
  (let [[x y] (exit grid)
        path (map first (a*-search
                         {:cost (fn [s _] (get grid (first s) 100))
                          :successors (fn [s] (successors-ultra grid s))
                          :sources [[[0 0] [0 1] 0]
                                    [[0 0] [1 0] 0]]
                          :goal? (fn [s] (= (first s) [x y]))}))]
    (let [pset (set path)]
      (doseq [j (range (inc y))]
        (println (apply str (for [i (range (inc x))]
                              (if (contains? pset [i j]) (get grid [i j]) "."))))))
    (println path)
    (- (path-cost grid path) (get grid [0 0]))))

(assert (= 94 (part2 (parse example))))
(assert (= 1017 (part2 (parse input))))
