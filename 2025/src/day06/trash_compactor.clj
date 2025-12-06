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

(defn parse2 [in]
  (str/split-lines in))

(defn vertical [rows idx]
  (->> (for [r rows
             :let [v (subs r idx (inc idx))]
             :when (re-find #"\d" v)]
         v)
       (apply str)
       parse-long))

(defn part2 [in]
  (let [ops (last in)
        rows (butlast in)]
    (loop [idx (dec (apply max (map count in))) group [] values []]
      (if (>= idx 0)
        (let [g (conj group (vertical rows idx))
              op (get {\+ + \* *} (nth ops idx nil))]
          (if op
            (recur (- idx 2)
                   []
                   (conj values (apply op g)))
            (recur (dec idx)
                   g
                   values)))
        (reduce + values)))))

(assert (= 3263827 (part2 (parse2 example))))
(assert (= 10695785245101 (part2 (parse2 input))))
