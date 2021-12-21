(ns dirac-dice
  (:require [clojure.string :as str]))

(defn parse [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [l] (mapv parse-long (rest (re-find #"Player (\d+) starting position: (\d+)" l)))))
       (into {})))

(defn init-state [s]
  (let [a (get s 1)
        b (get s 2)]
    {:turn 0
     :players {0 {:score 0 :p a}
               1 {:score 0 :p b}}}))

(init-state (parse "example"))

(defn add-track [a b]
  (inc (mod (+ (dec a) b) 10)))

(defn die-values [rolls]
  (+ (inc (mod rolls 100))
     (inc (mod (inc rolls) 100))
     (inc (mod (inc (inc rolls)) 100))))

(defn turn [{:keys [turn players] :as state}]
  (let [player (mod turn 2)
        {:keys [score p]} (get players player)
        advance (die-values (* turn 3))
        space (add-track p advance)]
    (-> state
        (update :turn inc)
        (assoc-in [:players player]
                  {:p space
                   :score (+ score space)}))))

(defn best-score [{:keys [players]}]
  (apply max (mapv :score (vals players))))

(defn part1 [filename]
  (last (take-while (fn [s] (<= (best-score s) 1000))
                    (iterate turn (init-state (parse filename))))))

(take 10 (iterate turn (init-state (parse "example"))))

(part1 "example")
