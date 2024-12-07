(ns day07.bridge-repair
  (:require [clojure.string :as str]))

(def input (slurp "src/day07/input"))
(def example (slurp "src/day07/example"))

(defn parse [in]
  (->> in
       str/split-lines
       (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line))))))

(defn tree [values]
  (loop [results [(first values)] expr (rest values)]
    (if-let [v (first expr)]
      (recur (concat (map (partial * v) results)
                     (map (partial + v) results))
             (rest expr))
      results)))

(comment (tree [1 2])
         (tree [1 2 3])
         (tree [81 40 27]))

(defn evaluate [[expected & values]]
  (some #{expected} (tree values)))

(defn part1 [equations]
  (reduce + (keep (fn [expr] (evaluate expr)) equations)))

;; (frequencies (map count (map rest (parse input))))

(assert (= 3749 (part1 (parse example))))
(assert (= 1985268524462 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
