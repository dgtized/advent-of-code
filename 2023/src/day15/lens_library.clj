(ns day15.lens-library
  (:require [clojure.string :as str]))

(def input (slurp "src/day15/input"))
(def example (slurp "src/day15/example"))

(defn parse [in]
  (str/split (str/trim in) #","))

(defn checksum [in]
  (reduce (fn [curr v]
            (rem (* 17 (+ curr (byte v))) 256))
          0
          (seq in)))

(assert (= 52 (checksum "HASH")))

(defn part1 [in]
  (apply + (map checksum in)))

(assert (= 1320 (part1 (parse example))))
(assert (= 515210 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
