(ns amphipod
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [lines (str/split-lines (slurp filename))]
    (into {} (for [[j line] (map-indexed vector lines)
                   [i c] (map-indexed vector (seq line))
                   :when (not= c \ )]
               [[i j] c]))))

(parse "result")

