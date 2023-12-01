(ns calibrate
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn part1 []
  (->> (slurp "day01/input")
       str/split-lines
       (map (fn [line] (keep (set (map (fn [d] (char (+ (byte \0) d))) (range 10))) (vec line))))
       (map (juxt first last))
       (map (fn [x] (edn/read-string (apply str x))))
       (reduce +)))

(comment
  (part1) ;; => 55447
  )

