(ns day07.camel-cards
  (:require [clojure.string :as str]))

(def input (slurp "src/day07/input"))
(def example (slurp "src/day07/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[hand bid] (str/split line #" ")]
      [hand (parse-long bid)])))

(defn rank-compare [a b]
  (println a b)
  0)

(defn part1 [in]
  (sort-by first rank-compare in))

(part1 (parse example))
(part1 (parse input))
