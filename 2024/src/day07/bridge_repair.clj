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

(defn evaluate [eval-tree [expected & values]]
  (some #{expected} (eval-tree values)))

(defn sum-tree [eval-tree equations]
  (reduce + (keep (fn [expr] (evaluate eval-tree expr)) equations)))

(defn part1 [equations]
  (sum-tree tree equations))

;; (frequencies (map count (map rest (parse input))))

(assert (= 3749 (part1 (parse example))))
(assert (= 1985268524462 (part1 (parse input))))

(defn tree-concat [[val & values]]
  (loop [results [val] expr values]
    (if-let [v (first expr)]
      (recur (concat (map (partial * v) results)
                     (map (partial + v) results)
                     (map (fn [e] (parse-long (str e v))) results))
             (rest expr))
      results)))

(defn part2 [equations]
  (sum-tree tree-concat equations))

(assert (= 11387 (part2 (parse example))))
;; slow 5565.96ms
(time (assert (= 150077710195188 (part2 (parse input)))))
