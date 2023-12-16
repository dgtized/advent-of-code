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

(defn part1 [grid]
  (loop [beams [(make-beam [-1 0] [1 0])]
         energized #{}
         steps 2000]
    (if (or (empty? beams) (<= steps 0))
      energized
      (recur (distinct (mapcat (partial beam-step grid) beams))
             (set/union energized (set (filter grid (map :pos beams))))
             (dec steps)))))

(assert (= 46 (count (part1 (parse example)))))
;; (assert (= 8901 (count (part1 (parse input)))))

(defn part2 [in]
  in)

;; (assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
