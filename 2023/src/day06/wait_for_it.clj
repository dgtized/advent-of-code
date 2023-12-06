(ns day06.wait-for-it
  (:require [clojure.string :as str]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn parse [in]
  (let [xs (map parse-long (re-seq #"\d+" in))
        [times dist] (split-at (/ (count xs) 2) xs)]
    (map vector times dist)))

(defn attempt [duration hold]
  (let [vel hold]
    (* vel (- duration hold))))

(defn cases [[time dist]]
  (for [hold (range time)
        :let [d (attempt time hold)]
        :when (> d dist)]
    [hold d]))

(defn part1 [in]
  (apply * (mapv (comp count cases) in)))

(assert (= 288 (part1 (parse example))))
(assert (= 4811940 (part1 (parse input))))
