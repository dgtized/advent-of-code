(ns day08.playground
  (:require [clojure.string :as str]
            [aoc.combinatorics :as ac]
            [clojure.math :as math]))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))

(defn distance [[x0 y0 z0] [x1 y1 z1]]
  (math/sqrt (+ (math/pow (- x1 x0) 2)
                (math/pow (- y1 y0) 2)
                (math/pow (- z1 z0) 2))))

(defn distances [junctions]
  (for [[a b] (ac/all-pairs junctions)]
    [[a b] (distance a b)]))

(defn parse [in]
  (let [locations
        (->> in
             str/split-lines
             (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line)))))]
    {:junctions locations
     :grid (into {} (distances locations))}))

(defn merge-graph [graph a b]
  (let [ma (get graph a)
        mb (get graph b)]
    (reduce-kv (fn [g l m]
                 (if (= m mb)
                   (-> g
                       (dissoc l)
                       (assoc l ma))
                   g))
               graph graph)))

(comment (merge-graph {[0 0 0] 1 [0 1 0] 2 [0 1 1] 3} [0 0 0] [0 1 0]))

(defn part1 [{:keys [junctions grid]} steps]
  (loop [conn 0 graph (into {} (map-indexed (fn [i loc] [loc i]) junctions)) connections (sort-by second grid)]
    (if (or (= conn steps)
            (= 1 (count (set (vals graph)))))
      graph
      (let [[[a b] _] (first connections)]
        (if (= (get graph a) (get graph b))
          (recur (inc conn)
                 graph
                 (rest connections))
          (recur (inc conn)
                 (merge-graph graph a b)
                 (rest connections)))))))

(defn count-graph [graph]
  (frequencies (vals graph)))

(defn score [fs]
  (apply * (take 3 (sort #(compare %2 %1) (mapv second fs)))))

(assert (= 40 (score (count-graph (part1 (parse example) 10)))))
(assert (= 97384 (score (count-graph (part1 (parse input) 1000)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
