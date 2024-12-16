(ns day16.reindeer-maze
  (:require
   [clojure.data.priority-map :as dpm]
   [aoc.grid :as ag]
   [clojure.string :as str]
   [aoc.vector :as v]
   [clojure.set :as set]))

(def input (slurp "src/day16/input"))
(def example (slurp "src/day16/example"))
(def example2 (slurp "src/day16/example2"))

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
   (a*-search {:successors (fn [s] (successors grid s))
               :cost (fn [[_ _ cost] _] cost)
               :sources [[(find-start grid) [1 0] 0]]
               :goal? (fn [s] (= (get grid (first s)) \E))} )))

(assert (= 7036 (path-cost (search-path (parse example)))))
(assert (= 106512 (path-cost (search-path (parse input)))))

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
(println (count (all-best-paths (parse input))))
;; (assert (= 563 (count (all-best-paths (parse input)))))
