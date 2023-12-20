(ns day20.pulse-propagation
  (:require [clojure.string :as str]))

(def input (slurp "src/day20/input"))
(def example (slurp "src/day20/example"))
(def example2 (slurp "src/day20/example2"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[module dests] (str/split line #" -> ")
          type (subs module 0 1)
          targets (str/split dests #",\s*")]
      (if (contains? #{"%" "&"} type)
        {:module (subs module 1)
         :type type
         :targets targets}
        {:module module
         :targets targets}))))

(defn part1 [in]
  in)

(assert (= (part1 (parse example))))
(assert (= (part1 (parse example2))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
