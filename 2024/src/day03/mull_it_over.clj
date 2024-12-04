(ns day03.mull-it-over
  (:require [clojure.string :as str]))

(def input (slurp "src/day03/input"))
(def example (slurp "src/day03/example"))
(def example2 (slurp "src/day03/example2"))

(defn parse [in]
  (for [m (re-seq #"mul\((\d+),(\d+)\)" in)]
    (let [[a b] (map parse-long (rest m))]
      [a b])))

(defn part1 [in]
  (apply + (map (fn [[a b]] (* a b)) in)))

(assert (= 161 (part1 (parse example))))
(assert (= 166630675 (part1 (parse input))))

(defn parse2 [in]
  (for [m (re-seq #"do\(\)|don\'t\(\)|mul\((\d+),(\d+)\)" in)]
    (cond
      (str/starts-with? (first m) "mul")
      (let [[a b] (map parse-long (rest m))]
        [a b])
      (= (first m) "do()")
      :on
      (= (first m) "don't()")
      :off)))

(defn part2 [input]
  (loop [in input acc 0 state true]
    (let [el (first in)]
      (cond (nil? el)
            acc
            (= el :on)
            (recur (rest in) acc true)
            (= el :off)
            (recur (rest in) acc false)
            :else
            (recur (rest in) (+ acc (if state (apply * el) 0)) state)))))

(assert (= 48 (part2 (parse2 example2))))
(assert (= 93465710 (part2 (parse2 input))))
