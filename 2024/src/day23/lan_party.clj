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
  (for [[n1 conns1] g
        n2 conns1
        :let [conns2 (get g n2)
              isec (set/intersection conns1 conns2)]
        :when (seq isec)
        n3 isec]
    (sort [n1 n2 n3])))

(defn part1 [in]
  (let [triples (dedupe (sort (map (partial str/join "-") (three-sets (peers in)))))]
    (filter #(re-find #"t" %) triples)))

(assert (= 7 (count (part1 (parse example)))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
