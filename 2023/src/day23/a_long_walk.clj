(ns day23.a-long-walk
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as dpm]))

(def input (slurp "src/day23/input"))
(def example (slurp "src/day23/example"))

(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn parse [in]
  (->grid (str/split-lines in)))

(defn exit [grid]
  [(dec (apply max (map first (keys grid))))
   (apply max (map second (keys grid)))])

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
                       (dpm/priority-map-keyfn-by first >)
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
                                          (> score (get-in queue [node 1]))))
                               (assoc queue node [(+ score (heuristic node)) score current])
                               queue)))
                         (pop queue)
                         (successors current))))))))

(defn v+ [a b] (mapv + a b))

(defn successors [grid pos]
  (for [dir (case (get grid pos)
              \> [[1 0]]
              \< [[-1 0]]
              \^ [[0 -1]]
              \v [[0 1]]
              [[1 0] [-1 0] [0 -1] [0 1]])
        :let [pos' (v+ pos dir)]
        :when (let [v (get grid pos')]
                (and v (not= v \#)))]
    pos'))

(comment (successors (parse example) [1 0]))

(defn draw-grid [grid]
  (let [[mx my] (map inc (exit grid))]
    (doseq [j (range my)]
      (println (apply str
                      (for [i (range mx)]
                        (get grid [i j])))))))

(defn part1 [grid]
  (let [path
        (->> {:successors (partial successors grid)
              :sources [[1 0]]
              :goal? #(= (exit grid) %)}
             a*-search)]
    (draw-grid (reduce (fn [g p] (assoc g p \O)) grid path))
    (count path)))

(assert (= (part1 (parse example))))
;; (assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
