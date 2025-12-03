(ns day03.lobby
  (:require [clojure.string :as str]))

(def input (slurp "src/day03/input"))
(def example (slurp "src/day03/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (map parse-long (re-seq #"\d" line))))

(defn first-index-of [value xs]
  (some (fn [[i v]] (when (= v value) i)) (map-indexed vector xs)))

(defn find-max [xs]
  (let [n (count xs)
        high (apply max xs)
        i (first-index-of high xs)]
    (if (= i (dec n))
      [(apply max (butlast xs)) high]
      [high (apply max (drop (inc i) xs))])))

(defn convert-sum [xs]
  (reduce + (mapv (fn [v] (parse-long (apply str v))) xs)))

(comment
  (find-max [9 8])
  (find-max [8 9])
  (find-max [8 1 9])
  (let [ex (nth (parse input) 29)]
    (find-max ex)))

(count (parse input))

(defn part1 [in]
  (mapv find-max in))

(comment
  (for [line (parse input)]
    (let [m (find-max line)
          ms (apply min m)]
      [m (remove (fn [x] (< x ms)) line)])))

(assert (= 357 (convert-sum (part1 (parse example)))))
;; not 17143
(assert (= 17343 (convert-sum (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
