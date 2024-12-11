(ns day11.plutonium-pebbles
  (:require [clojure.string :as str]))

(def input (slurp "src/day11/input"))
(def example (slurp "src/day11/example"))

(defn parse [in]
  (map parse-long (re-seq #"\d+" in)))

(defn expand [stone]
  (let [s (str stone)
        n (count s)]
    (cond (zero? stone)
          [1]
          (even? n)
          (let [half (/ n 2)]
            [(parse-long (subs s 0 half))
             (parse-long (subs s half (+ half half)))])
          :else
          [(* 2024 stone)])))

(comment (expand 0)
         (expand 12)
         (expand 123)
         (expand 1234)
         (expand 12345678))

(defn step [stones]
  (mapcat expand stones))

(comment (step (parse "0 1 10 99 999")))

(assert (= (parse "2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2")
           (nth (iterate step (parse "125 17")) 6)))

(defn part1 [in]
  (count (nth (iterate step in) 25)))

(assert (= 55312 (part1 (parse example))))
(assert (= 216042 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
