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

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
