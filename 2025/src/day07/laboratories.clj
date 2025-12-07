(ns day07.laboratories
  (:require
   [aoc.grid :as ag]
   [clojure.string :as str]))

(def input (slurp "src/day07/input"))
(def example (slurp "src/day07/example"))

(defn parse [in]
  (ag/lines->grid (str/split-lines in)))

(defn tachyon-splits [grid row columns]
  (reduce + (for [c columns]
              (case (get grid [c row])
                \^ 1
                \. 0))))

(defn propagate [grid row columns]
  (sort (set (flatten (for [c columns]
                        (case (get grid [c row])
                          \. [c]
                          \^ [(dec c) (inc c)]
                          []))))))

(defn part1 [grid]
  (let [[c _] (ag/some-value grid \S)
        [_ rows] (ag/dims grid)]
    (loop [row 1 columns #{c} splits 0]
      (if (= row rows)
        [columns splits]
        (recur (inc row)
               (propagate grid row columns)
               (+ splits (tachyon-splits grid row columns)))))))

(assert (= 21 (second (part1 (parse example)))))
(assert (= 1687 (second (part1 (parse input)))))

(defn timeline-splits [grid row columns]
  (reduce + (for [c columns]
              (case (get grid [c row])
                \^ 1
                \. 0))))

(defn part2 [grid]
  (let [[c _] (ag/some-value grid \S)
        [_ rows] (ag/dims grid)]
    (loop [row 1 columns {c 1}]
      (if (= row rows)
        columns
        (recur (inc row)
               (reduce (fn [cols c]
                         (case (get grid [c row])
                           \. cols
                           \^ (-> cols
                                  (update (dec c) (fnil + 0) (get columns c))
                                  (update (inc c) (fnil + 0) (get columns c) 0)
                                  (dissoc c))))
                       columns (keys columns)))))))

(assert (= 40 (reduce + (vals (part2 (parse example))))))
(assert (= 390684413472684 (reduce + (vals (part2 (parse input))))))
