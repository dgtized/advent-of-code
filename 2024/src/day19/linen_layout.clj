(ns day19.linen-layout
  (:require [clojure.string :as str]))

(def input (slurp "src/day19/input"))
(def example (slurp "src/day19/example"))

(defn parse [in]
  (let [[available patterns] (str/split in #"\n\n")]
    {:towels (str/split available #", ")
     :patterns (str/split-lines patterns)}))

(defn match [towels]
  (let [available (str/join "|" towels)
        p (re-pattern (str "(" available ")+"))]
    (fn [pattern]
      (re-matches p pattern))))

(defn part1 [{:keys [towels patterns]}]
  (count (keep (match towels) patterns)))

(assert (= 6 (part1 (parse example))))
(assert (= 265 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
