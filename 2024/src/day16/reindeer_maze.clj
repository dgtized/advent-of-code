(ns day16.reindeer-maze
  (:require
   [clojure.data.priority-map :as dpm]
   [aoc.grid :as ag]
   [clojure.string :as str]
   [aoc.vector :as v]))

(def input (slurp "src/day16/input"))
(def example (slurp "src/day16/example"))

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

(defn part1 [grid]
  (a*-search {:successors (fn [s] (successors grid s))
              :cost (fn [[_ _ cost] _] cost)
              :sources [[(find-start grid) [1 0] 0]]
              :goal? (fn [s] (= (get grid (first s)) \E))} ))

(assert (= 7036 (path-cost (part1 (parse example)))))
(assert (= 106512 (path-cost (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
