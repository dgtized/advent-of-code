(ns day01
  (:require [clojure.string :as str]))

(def input (slurp "src/day01/input"))
(def example (slurp "src/day01/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (map (fn [line] (map parse-long (re-seq #"\d+" line))))))

(defn part1 [in]
  (let [as (sort (map first in))
        bs (sort (map second in))]
    (reduce + (map (fn [a b] (abs (- a b))) as bs))))

(assert (= 11 (part1 (parse example))))
(assert (= 2285373 (part1 (parse input))))

(defn part2 [in]
  (let [bs (frequencies (map second in))]
    (reduce + (for [a (map first in)]
                (* a (get bs a 0))))))

(assert (= 31 (part2 (parse example))))
(assert (= 21142653 (part2 (parse input))))
