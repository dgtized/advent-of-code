(ns day15.warehouse-woes
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(def input (slurp "src/day15/input"))
(def example (slurp "src/day15/example"))

(defn moves->vec [moves]
  (->> moves
       (mapv (fn [move]
               (case move
                 \^ [0 -1]
                 \v [0 1]
                 \< [-1 0]
                 \> [1 0]
                 \newline nil)))
       (remove nil?)))

(defn parse [in]
  (let [[grid moves] (str/split in #"\n\n")
        board (ag/lines->grid (str/split-lines grid))
        pos (some (fn [[pos v]] (when (= v \@) pos)) board)]
    {:grid board
     :pos pos
     :moves (moves->vec moves)}))

(defn push [grid pos move]
  (let [piece (get grid pos)
        pos' (v/v+ pos move)
        ahead (get grid pos')]
    (case ahead
      \. [true
          (assoc grid
                 pos \.
                 pos' piece)
          pos']
      \# [false grid pos]
      \O (let [[ok grid' _] (push grid pos' move)]
           (if ok
             (into [true] (rest (push grid' pos move)))
             [false grid pos])))))

(defn display [grid]
  (let [ks (keys grid)
        mx (inc (apply max (map first ks)))
        my (inc (apply max (map second ks)))]
    (for [y (range my)]
      (apply str (for [x (range mx)]
                   (get grid [x y] 0))))))

(defn gps-score [grid]
  (reduce (fn [acc [[x y] v]]
            (if (= v \O)
              (+ acc (* 100 y) x)
              acc))
          0 grid))

(defn part1 [{:keys [grid pos moves]}]
  (let [[grid' _]
        (reduce (fn [[grid pos] move]
                  (into [] (rest (push grid pos move)))) [grid pos] moves)]
    grid'))

(assert (= 10092 (gps-score (part1 (parse example)))))
(assert (= 1552879 (gps-score (part1 (parse input)))))

(defn parse2 [in]
  (-> in
      parse
      (update :grid
              (fn [grid]
                (into {}
                      (mapcat (fn [[[x y] v]]
                                (let [[vl vr] (case v
                                                \. [\. \.]
                                                \# [\# \#]
                                                \O [\[ \]]
                                                \@ [\@ \.])]
                                  [[[(* 2 x) y] vl]
                                   [[(+ (* 2 x) 1) y] vr]]))
                              grid))))
      (update :pos (fn [[x y]] [(* 2 x) y]))))

(defn push2 [grid pos move]
  (if (pos? (abs (first move)))
    (let [piece (get grid pos)
          pos' (v/v+ pos move)
          ahead (get grid pos')]
      (case ahead
        \. [true (assoc grid
                        pos \.
                        pos' piece)
            pos']
        \# [false grid pos]
        \[ (let [[ok grid' _] (push2 grid pos' move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))
        \] (let [[ok grid' _] (push2 grid pos' move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))))
    (let [piece (get grid pos)
          pos' (v/v+ pos move)
          ahead (get grid pos')]
      (case ahead
        \. [true (assoc grid
                        pos \.
                        pos' piece)
            pos']
        \# [false grid pos]
        \[ (let [[ok grid' _] (push2 grid pos' move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))
        \] (let [[ok grid' _] (push2 grid pos' move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))))))

(defn part2 [in]
  in)

(assert (= (part2 (parse2 example))))
(assert (= (part2 (parse input))))
