(ns day12.christmas-tree-farm
  (:require [clojure.string :as str]))

(def input (slurp "src/day12/input"))
(def example (slurp "src/day12/example"))

(defn parse-shape [s]
  (let [[f & ls] (str/split-lines s)]
    [(parse-long (re-find #"\d+" f))
     (vec ls)]))

(defn parse-region [s]
  (let [[r v] (str/split s #": ")]
    {:size (mapv parse-long (re-seq #"\d+" r))
     :quantities (mapv parse-long (re-seq #"\d+" v))}))

(defn parse [in]
  (let [xs (str/split in #"\n\n")]
    {:shapes (mapv parse-shape (butlast xs))
     :regions (mapv parse-region (str/split-lines (last xs)))}))

(defn part1 [in]
  in)

(assert (= (part1 (parse example))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
