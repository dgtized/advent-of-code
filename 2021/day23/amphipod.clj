(ns amphipod
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as dpm]))

(defn parse [filename]
  (let [lines (str/split-lines (slurp filename))]
    (into {} (for [[j line] (map-indexed vector lines)
                   [i c] (map-indexed vector (seq line))
                   :when (not (#{\  \#} c))]
               [[i j] c]))))

(defn v+ [a b]
  (mapv + a b))

(defn neighbors [board pos]
  (for [change [[-1 0] [0 -1] [0 1] [1 0]]
        :let [neighbor (v+ pos change)
              value (get board neighbor)]
        :when value]
    [neighbor value]))

(assert (= [[[7 2] \B]]
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
  (defn expected-locs []
    expected)

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

(assert (set (pieces (parse "result"))))

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
  (let [all-open (remove #{[3 1] [5 1] [7 1] [9 1]} (open-spaces board))
        expected (expected-locs)]
    (for [[c v] (pieces board)
          :let [room (get expected v)
                constraints (if (corridor? c)
                              (fn [dest] (let [other (first (disj room dest))]
                                          (and (room dest) (#{\. v} (get board other)))))
                              (fn [dest] (or (corridor? dest)
                                            ((get (expected-locs) v) dest))))
                legal (keep (fn [dest]
                              (when-let [pathing (path board c dest)]
                                [dest (* (count pathing) (int (Math/pow 10 (- (int v) (int \A)))))]))
                            (filter constraints all-open))]
          :when (seq legal)]
      [c v (into {} legal)])))

(assert (= 4 (count (legal-moves (parse "example")))))

(defn ranked-moves [board]
  (->> (for [[src piece destinations] (legal-moves board)
             [dst cost] destinations]
         [src dst piece cost])
       (sort-by #(nth % 3))))

(assert (= 28 (count (ranked-moves (parse "example")))))

(defn move [board src dest]
  (let [v (get board src)]
    (assoc board dest v
           src \.)))

(defn solved? [board]
  (= 8 (count (correct-pieces board))))

(assert (solved? (parse "result")))

(defn solve [board moves]
  (cond (solved? board)
        [moves]
        (> (count moves) 8)
        []
        :else
        (->> (ranked-moves board)
             (remove (set moves))
             (mapcat (fn [[src dst piece cost]]
                       (solve (move board src dst) (conj moves [src dst piece cost])))))))

;; adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn search [board]
  (loop [visited {}
         queue (dpm/priority-map-keyfn first board [0 0 nil])]
    (when (seq queue)
      (let [[current [_ value prev]] (peek queue)
            visited' (assoc visited current prev)]
        (if (solved? current)
          (reverse (backtrack current visited'))
          (recur visited'
                 (reduce (fn [queue [node cost]]
                           (let [score (+ value cost)]
                             (if (and (not (contains? visited' node))
                                      (or (not (contains? queue node))
                                          (< score (get-in queue [node 1]))))
                               (assoc queue node [(+ score 1) score current])
                               queue)))
                         (pop queue)
                         (mapv (fn [[src dst _ cost]] [(move current src dst) cost])
                               (ranked-moves current)))))))))


;; (solve (parse "example") [])

(ranked-moves (move (parse "example") [7 2] [8 1]))

;; (search (parse "input"))
