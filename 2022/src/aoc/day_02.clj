(ns aoc.day-02
  (:require [clojure.string :as str]))

(defn file-lines [filename]
  (-> filename slurp str/split-lines))

;; test
(file-lines "input/day02.example")
(file-lines "input/day02.input")
