(ns aoc.day-02
  (:require [clojure.string :as str]
            [aoc.utility :refer [verify]]))

(defn parse [filename]
  (-> filename slurp str/split-lines))

(defn round [a b]
  (let [beats {"A" #{"C" "Z"}
               "B" #{"A" "X"}
               "C" #{"B" "Y"}
               "X" #{"C" "Z"}
               "Y" #{"A" "X"}
               "Z" #{"B" "Y"}}
        score {"X" 1
               "Y" 2
               "Z" 3}
        loses (get beats a)
        wins (get beats b)]
    (+ (get score b)
       (cond (contains? loses b)
             0
             (contains? wins a)
             6
             :else
             3))))

(defn cheat [a b]
  (let [beats {"A" "C"
               "B" "A"
               "C" "B"}
        move-score {"A" 1
                    "B" 2
                    "C" 3}
        score {"X" 0
               "Y" 3
               "Z" 6}
        loss (get beats a)
        move (case b
               "X" loss
               "Y" a
               "Z" (first (disj #{"A" "B" "C"} a loss)))]
    (+ (get move-score move)
       (get score b))))

(defn score [f input]
  (let [r (->> input
               (mapv (fn [line] (str/split line #"\s")))
               (map (partial apply f)))]
    [(apply + r) r]))

;; test
(verify (= [15 [8 1 6]] (score round (parse "input/day02.example"))))
(score round (parse "input/day02.input"))
(verify (= [12 [4 1 7]] (score cheat (parse "input/day02.example"))))
(score cheat (parse "input/day02.input"))
