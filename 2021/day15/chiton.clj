(ns chiton
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as dpm]))

(defn parse [filename]
  (into {} (for [[j line] (map-indexed vector (str/split-lines (slurp filename)))
                 [i v] (map-indexed (fn [i v] [i (parse-long (str v))]) (seq line))]
             [[i j] v])))

(defn dims [grid]
  (inc (apply max (map first (keys grid)))))

(defn neighbors [grid [x y]]
  (->> [[(+ x 1) y] [(- x 1) y] [x (+ y 1)] [x (- y 1)]]
       (filter grid)))

(defn cost [grid coord]
  (get grid coord))

(comment
  (neighbors (parse "example") [0 0])
  (cost (parse "example") [1 1]))

(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

;; adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn search [grid source target]
  (loop [visited {}
         queue (dpm/priority-map-keyfn first source [0 0 nil])]
    (when (seq queue)
      (let [[current [_ value prev]] (peek queue)
            visited' (assoc visited current prev)]
        (if (= current target)
          (reverse (backtrack target visited'))
          (recur visited'
                 (reduce (fn [queue node]
                           (let [score (+ value (cost grid node))]
                             (if (and (not (contains? visited' node))
                                      (or (not (contains? queue node))
                                          (< score (get-in queue [node 1]))))
                               (assoc queue node [(+ score 1) score current])
                               queue)))
                         (pop queue)
                         (neighbors grid current))))))))

(defn part1 [grid target]
  (rest (map (partial cost grid) (search grid [0 0] target))))

(assert (= 40 (apply + (part1 (parse "example") [9 9]))))
(assert (= 415 (apply + (part1 (parse "input") [99 99]))))
(assert (= 315 (apply + (part1 (parse "expanded") [49 49]))))

(defn incr-grid [grid [x y] increase]
  (into {}
        (for [[[i j] v] grid
              :let [nv (+ v increase)]]
          [[(+ x i) (+ y j)] (if (> nv 9) (- nv 9) nv)])))

(defn expand [grid]
  (let [size (dims grid)]
    (apply merge
           (for [i (range 5)
                 j (range 5)]
             (incr-grid grid [(* size i) (* size j)] (+ i j))))))

(defn map->grid
  [grid]
  (let [size (dims grid)]
    (->> (for [j (range size)
               i (range size)]
           (get grid [i j]))
         (partition size)
         (mapv vec))))

(assert (= (mapv (partial apply str) (map->grid (expand (parse "example"))))
           (mapv (partial apply str) (map->grid (parse "expanded")))))
(assert (= 315 (apply + (part1 (expand (parse "example")) [49 49]))))
(assert (= 2864 (apply + (part1 (expand (parse "input")) [499 499]))))

