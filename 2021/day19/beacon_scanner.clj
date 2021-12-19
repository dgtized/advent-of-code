(ns beacon-scanner
  (:require [clojure.string :as str]))

(defn parse [filename]
  (into {} (for [group (rest (str/split (slurp filename) #"--- scanner "))]
             [(parse-long (last (re-find #"(\d+) ---\n" group)))
              (->> group
                   str/split-lines
                   rest
                   (mapv (comp (partial mapv parse-long)
                               #(re-seq #"\-?\d+" %))))])))


