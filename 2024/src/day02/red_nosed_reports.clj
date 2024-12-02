(ns day02.red-nosed-reports
  (:require [clojure.string :as str]))

(def input (slurp "src/day02/input"))
(def example (slurp "src/day02/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))))

(defn safe? [report]
  (let [pairs (partition 2 1 report)]
    (and (every? (fn [[a b]] (<= 1 (abs (- b a)) 3)) pairs)
         (or (every? (fn [[a b]] (< a b)) pairs)
             (every? (fn [[a b]] (> a b)) pairs)))))

(defn part1 [in]
  (count (filter safe? in)))

(assert (= 2 (part1 (parse example))))
(assert (= 369 (part1 (parse input))))

(defn removals [report]
  (loop [i 0 acc []]
    (if (= i (count report))
      (conj acc report)
      (recur (inc i)
             (conj acc (vec (concat (take i report) (drop (inc i) report))))))))

(removals [1 2 3])

(defn safe-with-removal? [report]
  (some safe? (removals report)))

(defn part2 [in]
  (count (filter safe-with-removal? in)))

(assert (= 4 (part2 (parse example))))
(assert (= 428 (part2 (parse input))))
