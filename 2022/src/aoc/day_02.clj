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

(defn cheat [a b]
  (let [beats {"A" "C"
               "B" "A"
               "C" "B"}
        move-score {"A" 1
                    "B" 2
                    "C" 3}
        score {"X" 0
               "Y" 3
               "Z" 6}]
    (let [loss (get beats a)
          move (case b
                 "X" loss
                 "Y" a
                 "Z" (first (disj #{"A" "B" "C"} a loss)))]
      (+ (get move-score move)
         (get score b)))))

(defn score-part1 [input]
  (->> input
       (mapv (fn [line] (str/split line #"\s")))
       (map (partial apply round))
       (apply +)))

(defn score-part2 [input]
  (->> input
       (mapv (fn [line] (str/split line #"\s")))
       (map (partial apply cheat))
       (apply +)))

;; test
(score-part1 (parse "input/day02.example"))
(score-part1 (parse "input/day02.input"))
(score-part2 (parse "input/day02.example"))
(score-part2 (parse "input/day02.input"))

