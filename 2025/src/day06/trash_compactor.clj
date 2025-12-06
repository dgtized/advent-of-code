(ns day06.trash-compactor
  (:require [clojure.string :as str]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn transpose [m]
  (apply mapv vector m))

(defn parse [in]
  (->> in
       str/split-lines
       (mapv (fn [line]
               (let [row (re-seq #"\d+|[+*]" line)]
                 (if (contains? #{"+" "*"} (first row))
                   (vec row)
                   (mapv parse-long row)))))
       transpose
       (mapv (comp vec reverse))))

(defn calc [[op & args]]
  (apply (get {"+" + "*" *} op) args))

(defn part1 [in]
  (reduce + (mapv calc in)))

(assert (= 4277556 (part1 (parse example))))
(assert (= 4771265398012 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
