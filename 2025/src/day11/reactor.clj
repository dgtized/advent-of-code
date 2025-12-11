(ns day11.reactor
  (:require [clojure.string :as str]))

(def input (slurp "src/day11/input"))
(def example (slurp "src/day11/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [line]
              (let [[k v] (str/split line #": ")]
                [k (set (str/split v #"\s+"))])))
       (into {})))

(defn part1 [in]
  in)

(assert (= (part1 (parse example))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
