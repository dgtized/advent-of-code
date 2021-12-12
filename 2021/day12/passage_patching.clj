(ns passage-pathing
  (:require
   [clojure.string :as str]
   [loom.alg :as la]
   [loom.alg-generic :as lag]
   [loom.derived :as ld]
   [loom.graph :as lg]))

(defn big? [room]
  (= room (str/upper-case room)))

(defn small? [room]
  (= room (str/lower-case room)))

(defn parse [filename]
  (map #(str/split % #"-")
       (str/split-lines (slurp filename))))

(defn ->graph [edges]
  (apply lg/graph edges))

(defn start-to-end? [path]
  (and (= (first path) "start")
       (= (last path) "end")
       (not (some #{"start" "end"} (rest (butlast path))))))

(defn paths [successors path]
  (let [seen (set path)
        this-node (peek path)]
    (if (= this-node "end")
      [path]
      (->> (successors this-node)
           (remove (fn [n] (and (small? n) (seen n))))
           (mapcat #(paths successors (conj path %)))
           (cons path)))))

(defn legal-paths [g]
  (->> (paths (lg/successors g) ["start"])
       (filter start-to-end?)))

(assert (= 10 (count (legal-paths (->graph (parse "example1"))))))
(assert (= 19 (count (legal-paths (->graph (parse "example2"))))))
(assert (= 226 (count (legal-paths (->graph (parse "example3"))))))
(assert (= 5178 (count (legal-paths (->graph (parse "input"))))))

