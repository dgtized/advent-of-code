(ns day10.hoof-it
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(def input (slurp "src/day10/input"))
(def example (slurp "src/day10/example"))
(def example2 (slurp "src/day10/example2"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [line] (map parse-long (re-seq #"\d" line))))
       ag/lines->grid))

(defn find-pos [height grid]
  (->> grid
       (filter (fn [[cell val]] (when (= height val) cell)))))

(defn successors [grid pos]
  (for [dir v/cardinal
        :let [neighbor (v/v+ pos dir)
              height (get grid neighbor)]
        :when (and height (= (inc (get grid pos)) height))]
    neighbor))

(defn goal? [grid path]
  (= 9 (get grid (last path))))

(comment (find-pos 9 (parse example)))
(comment (find-pos 0 (parse example2))
         (count (find-pos 0 (parse input)))
         (count (find-pos 9 (parse input))))

(defn search [grid start]
  (loop [paths [[start]]]
    (cond (every? (partial goal? grid) paths)
          paths
          :else
          (recur (mapcat
                  (fn [path]
                    (for [neighbor (successors grid (last path))]
                      (conj path neighbor)))
                  paths)))))

(defn score-trail [grid start]
  (set (map last (search grid start))))

(comment (search (parse example) [0 0])
         (let [grid (parse example2)]
           (map #(score-trail grid %) (map first (find-pos 0 grid)))))

(defn score [method grid]
  (->> grid
       (find-pos 0)
       (map #(count (method grid (first %))))
       (apply +)))

(defn part1 [grid]
  (score score-trail grid))

(assert (= 1 (part1 (parse example))))
(assert (= 36 (part1 (parse example2))))
(assert (= 548 (part1 (parse input))))

(defn part2 [grid]
  (score search grid))

(assert (= 81 (part2 (parse example2))))
(assert (= 1252 (part2 (parse input))))
