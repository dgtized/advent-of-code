(ns day18.ram-run
  (:require
   [aoc.graph :as graph]
   [aoc.vector :as v]))

(def input (slurp "src/day18/input"))
(def example (slurp "src/day18/example"))

(defn parse [in]
  (map vec (partition 2 (map parse-long (re-seq #"\d+" in)))))

(defn successors [coords [mx my] pos]
  (for [dir v/cardinal
        :let [pos' (v/v+ pos dir)
              [x y] pos']
        :when (and (not (contains? coords pos'))
                   (<= 0 x mx)
                   (<= 0 y my))]
    pos'))

(assert (= [[1 0] [0 1]] (successors #{[1 1]} [6 6] [0 0])))

(defn part1 [coords bounds steps]
  (graph/a*-search
   {:successors (let [grid (set (take steps coords))]
                  (fn [pos] (successors grid bounds pos)))
    :sources [[0 0]]
    :goal? (fn [p] (= p bounds))}))

(assert (= 22 (dec (count (part1 (parse example) [6 6] 12)))))
(assert (= 438 (dec (count (part1 (parse input) [70 70] 1024)))))

(defn part2 [coords bounds]
  (time (some (fn [i] (when (part1 coords bounds i)
                       (nth coords i)))
              (reverse (range (count coords))))))

(assert (= [6 1] (part2 (parse example) [6 6])))
(assert (= [26 22] (part2 (parse input) [70 70])))

;; debug helper
(defn display [coords [mx my]]
  (let [grid (set coords)]
    (for [y (range (inc my))]
      (apply str (for [x (range (inc mx))]
                   (if (contains? grid [x y]) \# \.))))))

(comment (map println (display (take 12 (parse example)) [6 6])))
