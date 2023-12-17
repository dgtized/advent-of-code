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

(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

(defn a*-search
  ([successors source goal]
   (a*-search {} successors source goal))
  ([{:keys [cost heuristic] :or
     {cost (constantly 1)
      heuristic (constantly 1)}}
    successors source goal]
   (let [goal? (if (fn? goal)
                 goal
                 #(= % goal))]
     (loop [visited {}
            queue (dpm/priority-map-keyfn first source [0 0 nil])]
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
                            (successors current visited'))))))))))

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))

(map (fn [a b] (v- b a)) [[0 0] [1 0] [2 0]] [[1 0] [2 0] [3 0]])

(defn successors [grid pos visited]
  (let [path (reverse (take 4 (backtrack pos visited)))
        dirs (map v- (rest path) (butlast path))
        df (frequencies dirs)]
    (keep (fn [dir]
            (let [pos' (v+ pos dir)]
              (when (and (get grid pos')
                         (not= (last (butlast path)) pos')
                         (not= (get df dir 0) 3))
                pos')))
          [[1 0] [-1 0] [0 1] [0 -1]])))

(successors (parse example) [0 0] {})
(successors (parse example) [0 4] {[0 1] [0 0] [0 2] [0 1] [0 3] [0 2] [0 4] [0 3]})

(defn exit [grid]
  [(apply max (map first (keys grid)))
   (apply max (map second (keys grid)))])

(defn part1 [in]
  (let [[x y] (exit in)
        path (a*-search {:cost in} (partial successors in) [0 0] [x y])]
    (let [pset (set path)]
      (doseq [j (range (inc y))]
        (println (apply str (for [i (range (inc x))]
                              (if (contains? pset [i j]) (get in [i j]) "."))))))
    (apply + (map in path))))

(assert (= (part1 (parse example))))
;; (assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
