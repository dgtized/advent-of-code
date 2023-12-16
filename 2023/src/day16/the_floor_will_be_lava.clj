(ns day16.the-floor-will-be-lava
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "src/day16/input"))
(def example (slurp "src/day16/example"))

(defn parse [in]
  (into {}
        (for [[j line] (map-indexed vector (str/split-lines in))
              [i tile] (map-indexed vector line)]
          {[i j] tile})))


(defn v+ [a b] (mapv + a b))

(defn make-beam [pos dir]
  {:pos pos :dir dir})

(defn mirror [face dir]
  (get (if (= face \\)
         {[1 0] [0 1]
          [0 1] [1 0]
          [-1 0] [0 -1]
          [0 -1] [-1 0]}
         ;; \/
         {[1 0] [0 -1]
          [0 1] [-1 0]
          [-1 0] [0 1]
          [0 -1] [1 0]})
       dir))

(defn beam-step [grid {:keys [pos dir]}]
  (let [pos' (v+ pos dir)]
    (if-let [tile (get grid pos')]
      (case tile
        \. [(make-beam pos' dir)]
        \\ [(make-beam pos' (mirror \\ dir))]
        \/ [(make-beam pos' (mirror \/ dir))]
        \| (if (pos? (abs (second dir)))
             [(make-beam pos' dir)]
             [(make-beam pos' [0 1]) (make-beam pos' [0 -1])])
        \- (if (pos? (abs (first dir)))
             [(make-beam pos' dir)]
             [(make-beam pos' [1 0]) (make-beam pos' [-1 0])]))
      [])))

(defn part1 [grid start]
  (loop [beams [start]
         energized {}]
    (if (or (empty? beams))
      energized
      (recur (->> beams
                  (mapcat (partial beam-step grid))
                  (remove (fn [{:keys [pos dir]}]
                            (when-let [cell (get energized pos)]
                              (contains? cell dir)))))
             (reduce (fn [e {:keys [pos dir]}]
                       (if (get grid pos)
                         (update e pos (fnil conj #{}) dir)
                         e))
                     energized beams)))))

(def start-beam (make-beam [-1 0] [1 0]))
(assert (= 46 (count (part1 (parse example) start-beam))))
(time (assert (= 8901 (count (part1 (parse input) start-beam)))))

(defn starts [grid]
  (let [max-x (apply max (map first (keys grid)))
        max-y (apply max (map second (keys grid)))]
    (apply concat
           (keep (fn [[x y]]
                   (cond-> []
                     (= x 0) (conj (make-beam [x y] [1 0]))
                     (= x max-x) (conj (make-beam [x y] [-1 0]))
                     (= y 0) (conj (make-beam [x y] [0 1]))
                     (= y max-y) (conj (make-beam [x y] [0 -1]))))
                 (keys grid)))))

(starts (parse example))
;; (count (starts (parse input))) 440

(defn part2 [grid]
  (apply max
         (for [s (starts grid)]
           (count (part1 grid s)))))

(assert (= 51 (part2 (parse example))))
(time (assert (= 9064 (part2 (parse input)))))
