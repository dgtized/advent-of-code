(ns day04.scratchcards
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

(defn matches [{:keys [winners scratches]}]
  (count (set/intersection (set winners) (set scratches))))

(defn score [card]
  (let [m (matches card)]
    (if (pos-int? m)
      (int (Math/pow 2 (dec m)))
      0)))

(defn part1 [input]
  (apply + (map score (parse input))))

(defn part2 [input]
  (let [scratchcards (parse input)
        cards (zipmap (map :id scratchcards) (repeat 1))]
    (reduce
     (fn [cards {:keys [id] :as card}]
       (let [copies (get cards id)]
         (reduce (fn [cards dup]
                   (update cards (+ id dup) + copies))
                 cards
                 (range 1 (inc (matches card))))))
     cards
     scratchcards)))

(assert (= 13 (part1 example)))
(assert (= 21821 (part1 input)))

(assert (= 30 (apply + (vals (part2 example)))))
(assert (= 5539496 (apply + (vals (part2 input)))))
