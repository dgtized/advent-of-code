(ns aoc.day-02
  (:require [clojure.string :as str]))

(defn parse [filename]
  (-> filename slurp str/split-lines))

;; not invaliding cache?

(defn round [a b]
  (let [beats {"A" #{"C" "Z"}
               "B" #{"A" "X"}
               "C" #{"B" "Y"}
               "X" #{"C" "Z"}
               "Y" #{"A" "X"}
               "Z" #{"B" "Y"}}
        score {"X" 1
               "Y" 2
               "Z" 3}]
    (let [loses (get beats a)
          wins (get beats b)]
      (+ (get score b)
         (cond (contains? loses b)
               0
               (contains? wins a)
               6
               :else
               3)))))

(defn score [input]
  (->> input
       (mapv (fn [line] (str/split line #"\s")))
       (map (partial apply round))
       (apply +)))

;; test
(score (parse "input/day02.example"))
(score (parse "input/day02.input"))

