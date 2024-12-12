(ns day12.garden-groups
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(def input (slurp "src/day12/input"))
(def example (slurp "src/day12/example"))
(def example2 (slurp "src/day12/example2"))
(def example3 (slurp "src/day12/example3"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn neighbors [grid cell]
  (for [dir v/cardinal
        :let [loc (v/v+ cell dir)]
        :when (get grid loc)]
    loc))

(defn successors [cell]
  (for [dir v/cardinal] (v/v+ cell dir)))

(defn flood [grid start]
  (let [contents (get grid start)]
    (loop [region #{start} frontier (neighbors grid start)]
      (if (empty? frontier)
        region
        (let [accepted (filter #(= (get grid %) contents) frontier)
              region' (into region accepted)]
          (recur region'
                 (distinct (mapcat (fn [cell] (remove region' (neighbors grid cell))) accepted))))))))

(comment (flood (parse example2) [0 0]))

(defn segment [grid]
  (loop [regions [] grid grid]
    (if (empty? grid)
      regions
      (let [region (flood grid (ffirst grid))]
        (recur (conj regions region)
               (apply dissoc grid region))))))

(comment (segment (parse example))
         (segment (parse example2))
         (segment (parse example3)))

(defn perimeter [region]
  (reduce (fn [perimeter cell]
            (+ perimeter (count (remove region (successors cell)))))
          0
          region))

(comment (map perimeter (segment (parse example))))

(defn price [region]
  (* (count region) (perimeter region)))

(defn part1 [in]
  (apply + (map price (segment in))))

(assert (= 1930 (part1 (parse example))))
(assert (= 1431316 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
