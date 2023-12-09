(ns day09.mirage-maintenance
  (:require [clojure.string :as str]))

(def input (slurp "src/day09/input"))
(def example (slurp "src/day09/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [s] (map parse-long (re-seq #"-?\d+" s))))))

(defn sequences [history]
  (->> history
       (iterate (fn [s]
                  (for [[a b] (partition 2 1 s)]
                    (- b a))))
       (take-while #(not (every? (fn [v] (zero? v)) %)))))

(defn part1 [in]
  (->> in
       (map sequences)
       (map (fn [hs] (reduce (fn [acc v] (+ acc v )) (reverse (map last hs)))))
       (apply +)))

(assert (= 114 (part1 (parse example))))
(assert (= 1887980197 (part1 (parse input))))

(defn part2 [in]
  (->> in
       (map sequences)
       (map (fn [hs] (reduce (fn [acc v] (- v acc)) 0 (reverse (map first hs)))))
       (apply +)))

(assert (= 2 (part2 (parse example))))
(assert (= 990 (part2 (parse input))))
