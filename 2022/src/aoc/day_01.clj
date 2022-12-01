(ns aoc.day-01
  (:require [clojure.string :as str]))

(defn parse [filename]
  (str/split-lines (slurp filename)))

(parse "day01.in.txt")

