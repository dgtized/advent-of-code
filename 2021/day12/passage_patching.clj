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

(defn small-caves-once? [path]
  (let [f (frequencies path)]
    (not (some (fn [[n c]] (and (small? n) (> c 1))) f))))

(defn legal-paths [g]
  (filter (fn [path] (and (start-to-end? path) (small-caves-once? path)))
          (lag/paths (lg/successors g) ["start"])))

(assert (= 10 (count (legal-paths (->graph (parse "example1"))))))
;; (assert (= 19 (count (legal-paths (->graph (parse "example2"))))))
