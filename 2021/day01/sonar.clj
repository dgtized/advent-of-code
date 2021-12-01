(ns sonar
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def input
  (->> "input"
       slurp
       str/split-lines
       (mapv edn/read-string)))

(defn count> [xs]
  (count (filter (fn [[a b]] (> b a)) (partition 2 1 xs))))

(count> input) ;; => 1832
(count> (map #(reduce + %) (partition 3 1 input))) ;; => 1858

