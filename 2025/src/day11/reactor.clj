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

(defn bfs [graph]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY "you")
         paths []]
    (if (empty? queue)
      paths
      (let [current (peek queue)]
        (recur (into (pop queue) (get graph current))
               (conj paths current))))))

(defn part1 [in]
  (bfs in))

(defn score [in]
  (count (filter (fn [x] (= "out" x)) in)))

(assert (= 5 (score (part1 (parse example)))))
(assert (= 428 (score (part1 (parse input))))) 

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
