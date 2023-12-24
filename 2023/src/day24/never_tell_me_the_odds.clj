(ns day24.never-tell-me-the-odds
  (:require [clojure.string :as str]))

(def input (slurp "src/day24/input"))
(def example (slurp "src/day24/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (map (fn [p] (mapv parse-long (str/split p #",\s*")))
         (str/split line #"\s*@\s*"))))

(defn part1 [in]
  in)

(assert (= (part1 (parse example))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
