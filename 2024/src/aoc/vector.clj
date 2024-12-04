(ns aoc.vector)

(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn v* [v n]
  (mapv #(* % n) v))
