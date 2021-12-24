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
  (let [column {\A 3 \B 5 \C 7 \D 9}]
    (reduce (fn [acc [[x _] piece]]
              (+ acc (move-cost piece (+ (Math/abs (- x (column piece)))
                                         2))))
            0
            (incorrect-pieces expected board))))

(assert (= 0 (estimate-cost (expected (parse "result")) (parse "result"))))
(assert (= 6488 (estimate-cost (expected (parse "result")) (parse "example"))))

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

(assert (seq (path (parse "example") [7 2] [1 1])))
(assert (not (path (assoc (parse "example") [6 1] \#) [7 2] [1 1])))

(defn move [board src dest]
  (let [v (get board src)]
    (assoc board dest v
           src \.)))

(defn in-place? [expected board [c v]]
  (let [room (get expected v)]
    (and (room c)
         (every? #(= v (get board %))
                 (filter #(> (second %) (second c)) room)))))

(defn legal-moves [expected board]
  (let [all-open (remove #{[3 1] [5 1] [7 1] [9 1]} (open-spaces board))]
    (for [[c v] (pieces board)
          :when (not (in-place? expected board [c v]))
          :let [constraints (if (corridor? c)
                              (fn [dest]
                                (in-place? expected board [dest v]))
                              (fn [dest] (or (corridor? dest)
                                            ((get expected v) dest))))
                legal (keep (fn [dest]
                              (when-let [pathing (path board c dest)]
                                [dest (move-cost v (dec (count pathing)))]))
                            (filter constraints all-open))]
          [lm cost] legal]
      [c lm v cost])))

(assert (= 28 (count (legal-moves (expected (parse "result")) (parse "example")))))
(assert (empty? (legal-moves (expected (parse "result2")) (parse "result2"))))
(assert (= [[[4 1] [3 2] \A 2]]
           (legal-moves (expected (parse "result2")) (move (parse "result2") [3 2] [4 1]))))

(defn solved? [expected board]
  (= (apply set/union (vals expected))
     (set (mapv first (correct-pieces expected board)))))

(assert (solved? (expected (parse "result")) (parse "result")))
(assert (solved? (expected (parse "result2")) (parse "result2")))

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
                               (assoc queue
                                      node
                                      [(+ score (estimate-cost expected node)) score current [src dst cost]])
                               queue)))
                         (pop queue)
                         (mapv (fn [[src dst _ cost]] [(move current src dst) src dst cost])
                               (legal-moves expected current)))))))))

(assert (= 15 (count (legal-moves (expected (parse "result2")) (move (parse "example2") [7 2] [8 1])))))
(assert (search
         (expected (parse "result2"))
         (-> (parse "result2")
             (move [7 2] [8 1])
             (move [9 2] [7 2]))))

;; (search (parse "input"))
(assert (search (expected (parse "result2"))
                (move (parse "result2") [7 2] [8 1])))

(comment
  (def e1 (expected (parse "result")))
  (assert (= 12521 (:cost (time (search e1 (parse "example"))))))
  (assert (= 10607 (:cost (time (search e1 (parse "input"))))))

  (def e2 (expected (parse "result2")))
  (assert (= 44169 (:cost (time (search e2 (parse "example2"))))))
  (assert (= 44169 (:cost (time (search e2 (parse "input2")))))))
