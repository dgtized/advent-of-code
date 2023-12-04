(ns day04.day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day04/input"))
(def example (slurp "src/day04/example"))

(defn parse [input]
  (for [line (str/split-lines input)]
    (let [[card id] (re-find #"Card\s*(\d+):" line)
          [winners scratches] (str/split (subs line (count card)) #"\|")]
      {:id (parse-long id)
       :winners (mapv parse-long (re-seq #"\d+" winners))
       :scratches (mapv parse-long (re-seq #"\d+" scratches))})))

(defn score [{:keys [winners scratches]}]
  (let [matches (count (set/intersection (set winners) (set scratches)))]
    (if (pos-int? matches)
      (int (Math/pow 2 (dec matches)))
      0)))

(defn part1 [input]
  (apply + (map score (parse input))))

(defn part2 [input]
  )

(assert (= 13 (part1 example)))
(assert (= 21821 (part1 input)))
