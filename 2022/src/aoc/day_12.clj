(ns aoc.day-12
  (:require
   [aoc.utility :as aoc]
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

(defn path [grid src dst]
  (aoc/a*-search (partial neighbors grid) src dst))

(defn prepare [file]
  (let [grid (->grid (parse file))
        src (find-loc grid \S)
        dst (find-loc grid \E)
        grid (assoc grid src \a dst \z)]
    [grid src dst]))

(defn star1 [file]
  (let [[grid src dst] (prepare file)]
    (count (rest (path grid src dst)))))

(defn star2 [file]
  (let [[grid _ dst] (prepare file)]
    (apply min (remove #{0}
                       (map (fn [s] (count (rest (path grid s dst))))
                            (keep (fn [[pos v]] (when (= v \a) pos)) grid))))))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day12")
 (fn [{:keys [result]}] result))
