(ns day25.code-chronicles
  (:require [clojure.string :as str]
            [aoc.grid :as ag]))

(def input (slurp "src/day25/input"))
(def example (slurp "src/day25/example"))

(defn column [grid i]
  (sort-by first (for [[[x y] v] grid
                       :when (= x i)]
                   [y v])))

(defn parse [in]
  (for [pattern (str/split in #"\n\n")]
    (let [grid (->> pattern
                    str/split-lines
                    ag/lines->grid)
          columns (for [i (range 5)]
                    (apply str (map second (column grid i))))
          kind (if (= (ffirst columns) \#) :lock :key)]
      {:kind kind
       :pins (vec (for [c columns]
                    (case kind
                      :lock (count (take-while (fn [x] (= x \#)) (rest c)))
                      :key (- 6 (count (take-while (fn [x] (= x \.)) c))))))})))

(defn overlap? [lock key]
  (some (fn [v] (when (>= v 6) v)) (map + (:pins lock) (:pins key))))

(defn part1 [in]
  (let [{:keys [lock key]} (group-by :kind (sort-by :kind in))]
    (for [l lock
          k key
          :when (not (overlap? l k))]
      [l k])))

(assert (= 3 (count (part1 (parse example)))))
(assert (= 2854 (count (part1 (parse input)))))
