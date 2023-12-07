(ns day07.camel-cards
  (:require [clojure.string :as str]))

(def input (slurp "src/day07/input"))
(def example (slurp "src/day07/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[hand bid] (str/split line #" ")]
      [hand (parse-long bid)])))

(def cards [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])
(def card-rank (into {} (map vector cards (range))))

(defn rank [hand]
  (let [fr (frequencies (seq hand))
        [best best-n] (apply max-key second fr)]
    (cond (= best-n 5) 6
          (= best-n 4) 5
          (= best-n 3)
          (let [[_ b2n] (apply max-key second  (dissoc fr best))]
            (if (= b2n 2)
              4 ;; full house
              3));; three of a kind
          (= best-n 2)
          (let [[_ b2n] (apply max-key second  (dissoc fr best))]
            (if (= b2n 2)
              2 ;; two pair
              1)) ;; one pair
          :else 0) ;; high card
    ))

(map (fn [[hand _]] [hand (rank hand)]) (parse example))

(comment (rank "AAAAA")
         (rank "AA5AA")
         (rank "AAA55")
         (rank "AAA45"))

(defn rank-compare [a b]
  (let [c (compare (rank a) (rank b))]
    (if (zero? c)
      (some (fn [cp]
              (when-not (zero? cp)
                cp))
            (map (fn [ac bc]
                   (compare (get card-rank ac)
                            (get card-rank bc)))
                 a b))
      c)))

(defn score [ordered]
  (apply + (map-indexed (fn [i [_ bid]]
                          (* (inc i) bid))
                        ordered)))

(defn part1 [in]
  (sort-by first rank-compare in))

(assert (= 6440 (score (part1 (parse example)))))
(assert (= 250254244 )(score (part1 (parse input))))
