(ns day11.reactor
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day11/input"))
(def example (slurp "src/day11/example"))
(def example2 (slurp "src/day11/example2"))

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

(defn bfs-paths [{:keys [successors source goal]}]
  (loop [paths [[source]] visited #{source}]
    (if (contains? visited goal)
      (filter #(= (last %) goal) paths)
      (let [frontier (mapcat (fn [path]
                               (when (< (count path) 5)
                                 (for [n (successors (last path))]
                                   (when-not (contains? visited n)
                                     (conj path n)))))
                             paths)]
        (recur frontier (set/union visited (set (map last frontier))))))))

[#_(bfs-paths {:successors graph :source "svr" :goal "dac"})
 #_(bfs-paths {:successors graph :source "dac" :goal "fft"})
 #_(bfs-paths {:successors graph :source "fft" :goal "out"})]

(defn part2 [graph]
  [[(bfs-paths {:successors graph :source "svr" :goal "fft"})
    (bfs-paths {:successors graph :source "fft" :goal "dac"})
    #_(count (bfs-paths {:successors graph :source "dac" :goal "out"}))]])

(assert (= 2 (count (part2 (parse example2)))))
;; (assert (= (count (part2 (parse input)))))
