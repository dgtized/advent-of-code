(ns day23.lan-party
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day23/input"))
(def example (slurp "src/day23/example"))

(defn parse [in]
  (mapv rest (re-seq #"(\w+)-(\w+)" in)))

(defn peers [edges]
  (reduce (fn [peers [a b]]
            (-> peers
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {} edges))

(defn three-sets [g]
  (loop [nodes (keys g) triples #{}]
    (if (empty? nodes)
      triples
      (let [n1 (first nodes)
            conns1 (get g n1)]
        (recur
         (rest nodes)
         (into triples
               (for [n2 conns1
                     :let [isec (set/intersection conns1 (get g n2))]
                     :when (seq isec)
                     n3 isec]
                 (sort [n1 n2 n3]))))))))

(defn part1 [in]
  (->> in
       peers
       three-sets
       (map (partial str/join "-"))
       (filter #(re-find #"t\w" %))))

(assert (= 7 (count (sort (part1 (parse example))))))
(assert (= 1512 (count (part1 (parse input)))))

;; adapted from https://en.wikipedia.org/wiki/Clique_problem
;; and more specifically: https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
(defn bk-pivot [neighbors r p x]
  (if (and (empty? p) (empty? x))
    [(set r)]
    (let [pivot-u (first (concat p x))
          pivot-set (set/difference (set p) (neighbors pivot-u))]
      (loop [p p
             pivot-set pivot-set
             x x
             result []]
        (if (empty? pivot-set)
          result
          (let [v (first pivot-set)
                v-neighbors (neighbors v)]
            (recur (rest p) (rest pivot-set) (conj x v)
                   (into result
                         (bk-pivot neighbors
                                   (conj r v)
                                   (set/intersection (set p) v-neighbors)
                                   (set/intersection (set x) v-neighbors))))))))))

(defn part2 [edges]
  (let [g (peers edges)]
    (sort (apply max-key count (bk-pivot g #{} (set (keys g)) #{})))))

(assert (= "co,de,ka,ta" (str/join "," (part2 (parse example)))))
(assert (= "ac,ed,fh,kd,lf,mb,om,pe,qt,uo,uy,vr,wg"
           (str/join "," (part2 (parse input)))))
