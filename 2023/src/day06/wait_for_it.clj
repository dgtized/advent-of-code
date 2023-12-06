(ns day06.wait-for-it
  (:require [clojure.string :as str]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn parse [in]
  (let [xs (map parse-long (re-seq #"\d+" in))
        [times dist] (split-at (/ (count xs) 2) xs)]
    (map vector times dist)))

(parse example)
(parse input)
