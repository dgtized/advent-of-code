(ns passage-pathing
  (:require
   [clojure.string :as str]))

(defn small? [room]
  (= room (str/lower-case room)))

(defn parse [filename]
  (map #(str/split % #"-")
       (str/split-lines (slurp filename))))

(defn ->graph
  "Map node to set of successor nodes it is linked to."
  [edges]
  (reduce (fn [acc [a b]]
            (-> acc
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {} edges))

(defn part1
  ([successors] (part1 successors ["start"]))
  ([successors path]
   (let [seen (set path)
         this-node (peek path)]
     (if (= this-node "end")
       [path]
       (->> (successors this-node)
            (remove (fn [n] (and (small? n) (seen n))))
            (mapcat #(part1 successors (conj path %))))))))

(assert (= 10 (count (part1 (->graph (parse "example1"))))))
(assert (= 19 (count (part1 (->graph (parse "example2"))))))
(assert (= 226 (count (part1 (->graph (parse "example3"))))))
(assert (= 5178 (count (part1 (->graph (parse "input"))))))

(defn part2
  ([successors] (part2 successors ["start"]))
  ([successors path]
   (let [pf (frequencies path)
         seen (set path)
         seen-twice (some (fn [[n c]] (when (and (small? n) (> c 1)) n)) pf)
         this-node (peek path)]
     (if (= this-node "end")
       [path]
       (->> (successors this-node)
            (remove
             (fn [n]
               (when (seen n)
                 (or (= n "start")
                     (and (small? n) (or (> (get pf n 0) 1) seen-twice))))))
            (mapcat #(part2 successors (conj path %))))))))

(assert (= 36 (count (part2 (->graph (parse "example1"))))))
(assert (= 103 (count (part2 (->graph (parse "example2"))))))
(assert (= 3509 (count (part2 (->graph (parse "example3"))))))
(assert (= 130094 (count (part2 (->graph (parse "input"))))))
