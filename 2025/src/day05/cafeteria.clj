(ns day05.cafeteria
  (:require [clojure.string :as str]))

(def input (slurp "src/day05/input"))
(def example (slurp "src/day05/example"))

(defn parse [in]
  (let [[fresh ids] (str/split in #"\n\n")]
    {:fresh (for [m (re-seq #"(\d+)-(\d+)" fresh)]
              (mapv parse-long (rest m)))
     :ids (mapv parse-long (str/split-lines ids))}))

(defn fresh? [fresh id]
  (some (fn [[a b]] (when (<= a id b)
                     id))
        fresh))

(defn part1 [{:keys [fresh ids]}]
  (count (filter (partial fresh? fresh) ids)))

(assert (= 3 (part1 (parse example))))
(assert (= 635 (part1 (parse input))))

(defn range-ids [[a b]]
  (- (inc b) a))

(defn part2 [{:keys [fresh]}]
  (loop [ranges (sort-by first fresh) total 0]
    (cond (empty? ranges)
          total
          (= (count ranges) 1)
          (recur (rest ranges) (+ total (range-ids (first ranges))))
          :else
          (let [[r0 r1] (take 2 ranges)
                [a0 b0] r0
                [a1 b1] r1]
            (if (<= a0 a1 b0)
              (recur (cons [a0 (max b0 b1)] (drop 2 ranges)) total)
              (recur (rest ranges) (+ total (range-ids r0))))))))

(assert (= 14 (part2 (parse example))))
(assert (= 369761800782619 (part2 (parse input))))
