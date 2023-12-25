(ns day25.snowverload
  (:require [clojure.string :as str]))

(def input (slurp "src/day25/input"))
(def example (slurp "src/day25/example"))

(defn parse [in]
  (into {}
        (for [line (str/split-lines in)
              :let [[src & deps] (re-seq #"\w+" line)]]
          [src (vec deps)])))

(defn nodes [g]
  (reduce (fn [nodes [node conns]]
            (into (conj nodes node) conns))
          #{} g))

(defn edges [g]
  (reduce (fn [pairs [node conns]]
            (into pairs (map (partial vector node) conns)))
          #{} g))

(defn successors [g]
  (reduce (fn [sg [p q]]
            (-> sg
                (update p (fnil conj []) q)
                (update q (fnil conj []) p)))
          {} (edges g)))

(defn connected-set [successors start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start) seen #{}]
    (if-let [node (peek queue)]
      (recur (into (pop queue) (remove seen (successors node)))
             (conj seen node))
      seen)))

(defn part1 [in]
  [(count (nodes in))
   (count (edges in))
   (let [successor (successors in)]
     [(count (connected-set successor (first (nodes in))))
      (frequencies (map count (vals successor)))])])

(assert (= (part1 (parse example))))
;; 1475 nodes, 3312 edges, and between 4 and 9 edges
;; edge distribution {5 362, 4 957, 8 9, 6 119, 9 2, 7 26}
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
