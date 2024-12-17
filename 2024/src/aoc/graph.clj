(ns aoc.graph
  (:require [clojure.data.priority-map :as dpm]))

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
