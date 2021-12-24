(ns amphipod
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as dpm]
            [clojure.set :as set]))

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

(defn expected [input]
  (update-vals (group-by second (pieces input))
               #(set (map first %))))

(assert (expected (parse "result2")))

(defn correct-pieces [expected board]
  (reduce (fn [s [p v]] (if ((get expected v) p)
                         (conj s [p v])
                         s))
          #{}
          (sort-by second (pieces board))))

(defn incorrect-pieces [expected board]
  (set/difference (set (pieces board))
                  (correct-pieces expected board)))

(defn move-cost [piece spaces]
  (* spaces (int (Math/pow 10 (- (int piece) (int \A))))))

(defn estimate-cost [expected board]
  (reduce (fn [acc [_ piece]] (+ acc (move-cost piece 2)))
          0
          (incorrect-pieces expected board)))

(assert (= 0 (estimate-cost (expected (parse "result")) (parse "result"))))
(assert (= 2242 (estimate-cost (expected (parse "result")) (parse "example"))))

(defn corridor? [[x y]]
  (= y 1))

(assert (= #{[[3 3] \A] [[9 2] \D] [[7 3] \C]}
           (correct-pieces (expected (parse "result")) (parse "example"))))

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

(defn legal-moves [expected board]
  (let [all-open (remove #{[3 1] [5 1] [7 1] [9 1]} (open-spaces board))]
    (for [[c v] (pieces board)
          :let [room (get expected v)
                constraints (if (corridor? c)
                              (fn [dest]
                                (and (room dest)
                                     (every? (fn [other] #{\. v} (get board other))
                                             (disj room dest))))
                              (fn [dest] (or (corridor? dest)
                                            ((get expected v) dest))))
                legal (keep (fn [dest]
                              (when-let [pathing (path board c dest)]
                                [dest (move-cost v (dec (count pathing)))]))
                            (filter constraints all-open))]
          :when (seq legal)]
      [c v (into {} legal)])))

(assert (= 4 (count (legal-moves (expected (parse "result")) (parse "example")))))

(defn ranked-moves [expected board]
  (->> (for [[src piece destinations] (legal-moves expected board)
             [dst cost] destinations]
         [src dst piece cost])
       (sort-by #(nth % 3))))

(assert (= 28 (count (ranked-moves (expected (parse "result")) (parse "example")))))

(defn move [board src dest]
  (let [v (get board src)]
    (assoc board dest v
           src \.)))

(defn solved? [expected board]
  (= (apply set/union (vals expected))
     (set (mapv first (correct-pieces expected board)))))

(assert (solved? (expected (parse "result")) (parse "result")))

(defn solve [expected board moves]
  (cond (solved? expected board)
        [moves]
        (> (count moves) 8)
        []
        :else
        (->> (ranked-moves board)
             (remove (set moves))
             (mapcat (fn [[src dst piece cost]]
                       (solve expected (move board src dst) (conj moves [src dst piece cost])))))))

;; adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn search [expected board]
  (loop [visited {}
         path {}
         queue (dpm/priority-map-keyfn first board [0 0 nil []])]
    (when (seq queue)
      (let [[current [_ value prev mpath]] (peek queue)
            visited' (assoc visited current prev)
            path' (assoc path current mpath)]
        (if (solved? expected current)
          (let [final (rest (map path' (reverse (backtrack current visited'))))]
            {:path final
             :cost (apply + (mapv #(nth % 2) final))
             :states (count visited')})
          (recur visited'
                 path'
                 (reduce (fn [queue [node src dst cost]]
                           (let [score (+ value cost)]
                             (if (and (not (contains? visited' node))
                                      (or (not (contains? queue node))
                                          (< score (get-in queue [node 1]))))
                               (assoc queue node [(+ score (estimate-cost expected current)) score current [src dst cost]])
                               queue)))
                         (pop queue)
                         (mapv (fn [[src dst _ cost]] [(move current src dst) src dst cost])
                               (ranked-moves expected current)))))))))


;; (solve (parse "example") [])

(ranked-moves (expected (parse "result2")) (move (parse "example2") [7 2] [8 1]))

(assert (search
         (expected (parse "result"))
         (-> (parse "result")
             (move [7 2] [8 1])
             (move [9 2] [7 2]))))

;; (search (parse "input"))
(assert (search (expected (parse "result2"))
                (move (parse "result2") [7 2] [8 1])))

(comment
  (let [expected (expected (parse "result"))]
    (assert (= 12521 (:cost (search expected (parse "example")))))
    (assert (= 10607 (:cost (search expected (parse "input"))))))

  (def e2 (expected (parse "result2")))
  (assert (= 44169 (:cost (search e2 (parse "example2"))))))
