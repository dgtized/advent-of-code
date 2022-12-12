(ns aoc.day-12
  (:require
   [aoc.utility :as aoc]
   [clojure.set :as set]
   [clojure.data.priority-map :as dpm]
   [nextjournal.clerk :as clerk]))

#_(aoc/day-input :day 12 :name "example" :content "")
#_(aoc/day-input :day 12)

{::clerk/visibility {:result :hide}}

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (vec (seq line))))

(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn find-loc [grid value]
  (some (fn [[loc v]] (when (= v value) loc)) grid))

(find-loc (->grid (parse "input/day12.example")) \S)

(defn v+ [a b] (map + a b))

(defn height [x]
  (- (int x) (int \a)))

(defn neighbors [grid pos]
  (for [dir [[-1 0] [0 -1] [0 1] [1 0]]
        :let [neighbor (v+ pos dir)
              curr (get grid pos)
              value (get grid neighbor)]
        :when (and value
                   (<= (height value) (inc (height curr))))]
    neighbor))

;; originally adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

(defn a*-search [successors cost heuristic source target]
  (loop [visited {}
         queue (dpm/priority-map-keyfn first source [0 0 nil])]
    (when (seq queue)
      (let [[current [_ value prev]] (peek queue)
            visited' (assoc visited current prev)]
        (if (= current target)
          (reverse (backtrack target visited'))
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

(defn star1 [file]
  (let [grid (->grid (parse file))
        src (find-loc grid \S)
        dst (find-loc grid \E)
        grid (assoc grid src \a dst \z)
        path (partial a*-search (partial neighbors grid) (constantly 1) (constantly 1))]
    (count (rest (path src dst)))))

(defn star2 [file]
  (let [grid (->grid (parse file))
        src (find-loc grid \S)
        dst (find-loc grid \E)
        grid (assoc grid src \a dst \z)
        path (partial a*-search (partial neighbors grid) (constantly 1) (constantly 1))]
    (apply min (remove #{0}
                       (map (fn [s] (count (rest (path s dst))))
                            (keep (fn [[pos v]] (when (= v \a) pos)) grid))))))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day12")
 (fn [{:keys [result]}] result))
