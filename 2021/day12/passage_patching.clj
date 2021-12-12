(ns passage-pathing
  (:require
   [clojure.string :as str]))

(defn big? [room]
  (= room (str/upper-case room)))

(defn small? [room]
  (= room (str/lower-case room)))

(defn parse [filename]
  (map #(str/split % #"-")
       (str/split-lines (slurp filename))))

;; (defn ->graph [edges]
;;   (apply lg/graph edges))

(defn ->graph
  "Map node to set of successor nodes it is linked to."
  [edges]
  (reduce (fn [acc [a b]]
            (-> acc
                (update a (fnil conj #{}) b)
                (update b (fnil conj #{}) a)))
          {} edges))

(defn paths [successors path]
  (let [seen (set path)
        this-node (peek path)]
    (if (= this-node "end")
      [path]
      (->> (successors this-node)
           (remove (fn [n] (and (small? n) (seen n))))
           (mapcat #(paths successors (conj path %)))))))

(defn legal-paths [g]
  (paths g ["start"]))

(assert (= 10 (count (legal-paths (->graph (parse "example1"))))))
(assert (= 19 (count (legal-paths (->graph (parse "example2"))))))
(assert (= 226 (count (legal-paths (->graph (parse "example3"))))))
(assert (= 5178 (count (legal-paths (->graph (parse "input"))))))

(defn paths2 [successors path]
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
           (mapcat #(paths2 successors (conj path %)))))))

(defn part2 [g]
  (paths2 g ["start"]))

(assert (= 36 (count (part2 (->graph (parse "example1"))))))
(assert (= 103 (count (part2 (->graph (parse "example2"))))))
(assert (= 3509 (count (part2 (->graph (parse "example3"))))))
(assert (= 130094 (count (part2 (->graph (parse "input"))))))
