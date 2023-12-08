(ns day08.haunted-wasteland
  (:require [clojure.string :as str]))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))
(def example2 (slurp "src/day08/example2"))

(defn parse [in]
  (let [lines (str/split-lines in)]
    {:dirs (first lines)
     :maps
     (into {}
           (for [line (drop 2 lines)]
             (let [[a b c] (re-seq #"[A-Z]+" line)]
               [a {\L b \R c}])))}))

(defn path [{:keys [dirs maps]}]
  (loop [pos "AAA" path [] dirs (cycle (seq dirs))]
    (if (= pos "ZZZ")
      path
      (recur (get-in maps [pos (first dirs)])
             (conj path pos)
             (rest dirs)))))

(defn part1 [in]
  (count (path in)))

(assert (= 2 (part1 (parse example))))
(assert (= 6 (part1 (parse example2))))
(assert (= 11309 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
