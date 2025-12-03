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

(defn find-best [value limit xs]
  (let [i (first-index-of value xs)]
    (cond (= value 0)
          nil
          (nil? i)
          (recur (dec value) limit xs)
          (<= (+ i limit) (count xs))
          value
          :else
          (recur (dec value) limit xs))))

(comment
  (find-best 9 2 [8 7 9])
  (find-best 9 1 [7 9 8])
  (find-best 9 3 [8 7 9])
  (find-best 9 2 [3 2 1]))

(defn find-max-el
  ([xs] (find-max-el [] 12 xs))
  ([accept limit xs]
   (if (= (count accept) 12)
     accept
     (let [best (find-best (apply max xs) limit xs)]
       (if (some? best)
         (recur (conj accept best)
                (dec limit)
                (drop (inc (first-index-of best xs)) xs))
         [:fail xs])))))

(defn part2 [in]
  (mapv find-max-el in))

(assert (= 3121910778619 (convert-sum (part2 (parse example)))))
(assert (= 172664333119298 (convert-sum (part2 (parse input)))))
