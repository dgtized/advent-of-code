(ns aoc.vector)

(def cardinal
  "N E S W"
  [[0 -1] [1 0] [0 1] [-1 0]])
(def ordinal
  "NE SE SW NW"
  [[1 -1] [1 1] [-1 1] [-1 -1]])
(def directions
  "N NE E SE S SW W NW"
  (mapcat vector cardinal ordinal))

(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn v- [v1 v2]
  (mapv - v1 v2))

(defn v* [v n]
  (mapv #(* % n) v))
