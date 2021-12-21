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
     :players {0 {:p a :score 0}
               1 {:p b :score 0}}}))

(init-state (parse "example"))

(defn add-track [a b]
  (inc (mod (+ (dec a) b) 10)))

(defn die-values [rolls]
  [(inc (mod rolls 100))
   (inc (mod (inc rolls) 100))
   (inc (mod (inc (inc rolls)) 100))])

(die-values (* 3 34))

(defn turn [{:keys [turn players] :as state}]
  (let [player (mod turn 2)
        {:keys [score p]} (get players player)
        advance (apply + (die-values (* turn 3)))
        space (add-track p advance)]
    (-> state
        (update :turn inc)
        (assoc-in [:players player]
                  {:p space
                   :score (+ score space)}))))

(defn best-score [{:keys [players]}]
  (apply max (mapv :score (vals players))))

(defn part1-score [{:keys [turn players]}]
  (let [min-score (apply min (mapv :score (vals players)))
        rolls (* turn 3)]
    (* min-score rolls)))

(defn part1 [filename]
  (->> filename
       parse
       init-state
       (iterate turn)
       (drop-while (fn [s] (< (best-score s) 1000)))
       first
       part1-score))

;; (take 12 (iterate turn (init-state (parse "example"))))

(assert (= 739785 (part1 "example")))
(assert (= 929625 (part1 "input")))
