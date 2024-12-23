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
                 (str/join "-" (sort [n1 n2 n3])))))))))

(defn part1 [in]
  (let [triples (three-sets (peers in))]
    (filter #(re-find #"t\w" %) triples)))

(assert (= 7 (count (sort (part1 (parse example))))))
(assert (= 1512 (count (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
