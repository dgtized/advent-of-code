(ns amphipod
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as dpm]))

(defn parse [filename]
  (let [lines (str/split-lines (slurp filename))]
    (into {} (for [[j line] (map-indexed vector lines)
                   [i c] (map-indexed vector (seq line))
                   :when (not= c \ )]
               [[i j] c]))))

(defn v+ [a b]
  (mapv + a b))

(defn neighbors [board pos]
  (for [change [[-1 0] [0 -1] [0 1] [1 0]]
        :let [neighbor (v+ pos change)
              value (get board neighbor)]
        :when value]
    [neighbor value]))

(assert (= [[[6 3] \#] [[7 2] \B] [[7 4] \#] [[8 3] \#]]
           (neighbors (parse "example") [7 3])))

(defn pieces [board]
  (filter (fn [[_ v]] (#{\A \B \C \D} v)) board))

(defn movable-pieces [board]
  (filter (fn [[p _]]
            (some (fn [[_ val]] (= \. val)) (neighbors board p)))
          (pieces board)))

(assert (= [[[7 2] \B] [[5 2] \C] [[9 2] \D] [[3 2] \B]]
           (movable-pieces (parse "example"))))

(defn open-spaces [board]
  (mapv first (filter (fn [[_ v]] (= \. v)) board)))

(assert (= 11 (count (open-spaces (parse "example")))))

(let [expected (update-vals (group-by second (pieces (parse "result")))
                            #(set (map first %)))]
  (defn correct-pieces [board]
    (reduce (fn [s [p v]] (if ((get expected v) p)
                           (conj s [p v])
                           s))
            #{}
            (sort-by second (pieces board)))))

(defn corridor? [[x y]]
  (= y 1))

(assert (= #{[[3 3] \A] [[9 2] \D] [[7 3] \C]}
           (correct-pieces (parse "example"))))

(defn open-neighbors [grid p]
  (mapv first (filter (fn [[_ val]] (= val \.)) (neighbors grid p))))

(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

;; adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn path [grid source target]
  (loop [visited {}
         queue (dpm/priority-map-keyfn first source [0 0 nil])]
    (when (seq queue)
      (let [[current [_ value prev]] (peek queue)
            visited' (assoc visited current prev)]
        (if (= current target)
          (reverse (backtrack target visited'))
          (recur visited'
                 (reduce (fn [queue node]
                           (let [score (+ value 1)]
                             (if (and (not (contains? visited' node))
                                      (or (not (contains? queue node))
                                          (< score (get-in queue [node 1]))))
                               (assoc queue node [(+ score 1) score current])
                               queue)))
                         (pop queue)
                         (open-neighbors grid current))))))))

(assert (not (path (assoc (parse "example") [6 1] \#) [7 2] [1 1])))

(defn legal-moves [board]
  (let [all-open (remove #{[3 1] [5 1] [7 1] [9 1]} (open-spaces board))]
    (for [[c v] (pieces board)
          :let [constraints (if (corridor? c)
                              (complement corridor?)
                              corridor?)
                legal (keep (fn [dest]
                              (when-let [pathing (path board c dest)]
                                [dest (* (count pathing) (int (Math/pow 10 (- (int v) (int \A)))))]))
                            (filter constraints all-open))]
          :when (seq legal)]
      [c v (into {} legal)])))

(legal-moves (parse "example"))

