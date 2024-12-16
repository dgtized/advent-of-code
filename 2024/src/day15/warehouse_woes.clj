(ns day15.warehouse-woes
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(def input (slurp "src/day15/input"))
(def example (slurp "src/day15/example"))
(def example2 (slurp "src/day15/example2"))

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

(defn run [push {:keys [grid pos moves]}]
  (let [[grid' _]
        (reduce (fn [[grid pos] move]
                  (into [] (rest (push grid pos move))))
                [grid pos] moves)]
    grid'))

(assert (= 10092 (gps-score (run push (parse example)))))
(assert (= 1552879 (gps-score (run push (parse input)))))

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

(defn push-pair [grid p1 p2 move]
  (let [piece1 (get grid p1)
        piece2 (get grid p2)
        p1' (v/v+ p1 move)
        p2' (v/v+ p2 move)
        ahead1 (get grid p1')
        ahead2 (get grid p2')]
    (case [ahead1 ahead2]
      [\. \.]
      [true (assoc grid
                   p1 \. p2 \.
                   p1' piece1
                   p2' piece2)]
      [\] \.]
      (let [[ok grid'] (push-pair grid (v/v+ p1' [-1 0]) p1' move)]
        (if ok
          [true (second (push-pair grid' p1 p2 move))]
          [false grid]))
      [\. \[]
      (let [[ok grid'] (push-pair grid p2' (v/v+ p2' [1 0]) move)]
        (if ok
          [true (second (push-pair grid' p1 p2 move))]
          [false grid]))
      [\] \[]
      (let [[ok-l grid-l] (push-pair grid (v/v+ p1' [-1 0]) p1' move)]
        (if ok-l
          (let [[ok-r grid-r] (push-pair grid-l p2' (v/v+ p2' [1 0]) move)]
            (if ok-r
              [true (second (push-pair grid-r p1 p2 move))]
              [false grid]))
          [false grid]))
      [\[ \]]
      (let [[ok grid'] (push-pair grid p1' p2' move)]
        (if ok
          [true (second (push-pair grid' p1 p2 move))]
          [false grid]))
      [false grid])))

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
        \[ (let [[ok grid' _] (push-pair grid pos' (v/v+ pos' [1 0]) move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))
        \] (let [[ok grid' _] (push-pair grid (v/v+ pos' [-1 0]) pos' move)]
             (if ok
               (into [true] (rest (push2 grid' pos move)))
               [false grid pos]))))))

(defn gps-score2 [grid]
  (reduce (fn [acc [[x y] v]]
            (if (= v \[)
              (+ acc (* 100 y) x)
              acc))
          0 grid))

(comment
  (doseq [step (range 13)]
    (println step)
    (mapv println (display (run push2 (update (parse2 example2) :moves (partial take step))))))
  (display (run push2 (parse2 example2)))
  (display (run push2 (parse2 example))))

(assert (= 9021 (gps-score2 (run push2 (parse2 example)))))
(assert (= 618 (gps-score2 (run push2 (parse2 example2)))))
(assert (= 1561175 (gps-score2 (run push2 (parse2 input)))))
