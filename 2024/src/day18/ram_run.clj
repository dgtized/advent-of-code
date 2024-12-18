(ns day18.ram-run
  (:require [clojure.string :as str]
            [aoc.vector :as v]
            [aoc.grid :as ag]
            [aoc.graph :as graph]))

(def input (slurp "src/day18/input"))
(def example (slurp "src/day18/example"))

(defn parse [in]
  (map vec (partition 2 (map parse-long (re-seq #"\d+" in)))))

(defn display [coords [mx my]]
  (let [grid (set coords)]
    (for [y (range (inc my))]
      (apply str (for [x (range (inc mx))]
                   (if (contains? grid [x y]) \# \.))))))

(comment (map println (display (take 12 (parse example)) [6 6])))

(defn successors [coords [mx my] pos]
  (for [dir v/cardinal
        :let [pos' (v/v+ pos dir)
              [x y] pos']
        :when (and (not (contains? coords pos'))
                   (<= 0 x mx)
                   (<= 0 y my))]
    pos'))

(comment (successors #{[1 1]} [6 6] [0 0]))

(defn part1 [coords bounds]
  (let [grid (set coords)]
    (graph/a*-search {:successors (fn [pos] (successors grid bounds pos))
                      :sources [[0 0]]
                      :goal? (fn [p] (= p bounds))})))

(assert (= 22 (dec (count (part1 (take 12 (parse example)) [6 6])))))
(assert (= 438 (dec (count (part1 (take 1024 (parse input)) [70 70])))))

(defn part2 [coords bounds]
  (time (loop [i (count coords)]
          (if (and (> i 0) (nil? (part1 (take i coords) bounds)))
            (recur (dec i))
            (nth coords i)))))

(assert (= [6 1] (part2 (parse example) [6 6])))
(assert (= [26 22] (part2 (parse input) [70 70])))
