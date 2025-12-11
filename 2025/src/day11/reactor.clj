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

(defn reactor-paths [{:keys [successors source points goal]}]
  (let [mem (atom {})]
    (letfn [(path-count [loc passed]
              (if-let [e (find @mem [loc passed])]
                (val e)
                (let [ret (if (= loc goal)
                            (if (= passed points) 1 0)
                            (let [passed' (if (points loc) (conj passed loc) passed)]
                              (reduce + (mapv (fn [l] (path-count l passed')) (successors loc)))))]
                  (swap! mem assoc [loc passed] ret)
                  ret)))]
      (path-count source #{}))))

(defn part2 [graph]
  (reactor-paths {:successors graph :source "svr" :goal "out" :points #{"fft" "dac"}}))

(assert (= 2 (part2 (parse example2))))
(assert (= 331468292364745 (part2 (parse input))))
