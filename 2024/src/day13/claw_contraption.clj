(ns day13.claw-contraption
  (:require [clojure.string :as str]
            [aoc.vector :as v]
            [clojure.math :as math]))

(def input (slurp "src/day13/input"))
(def example (slurp "src/day13/example"))

(defn parse [in]
  (for [game (str/split in #"\n\n")]
    (let [[ax ay bx by gx gy] (mapv parse-long (re-seq #"\d+" game))]
      {:a [ax ay] :b [bx by] :goal [gx gy]})))

(defn calc-max [{[ax ay] :a [bx by] :b [gx gy] :goal :as game}]
  (assoc game
         :max
         {:a [(quot gx ax) (quot gy ay)]
          :ma (apply min [(quot gx ax) (quot gy ay)])
          :b [(quot gx bx) (quot gy by)]}))

(defn search [limit {:keys [a b goal max]}]
  (keep identity
        (for [push-a (range 0 (if limit limit (:ma max)))]
          (let [goal' (v/v- goal (v/v* a push-a))
                bx (/ (first goal') (first b))
                by (/ (second goal') (second b))]
            (when (= bx by)
              {:a push-a
               :b bx
               :cost (+ (* push-a 3) bx)})))))

(defn solve [game]
  (let [game' (calc-max game)
        solutions (search 100 game')
        solve-max (search nil game')]
    (assoc game'
           :solutions solutions
           :solve-max solve-max
           :min-cost (if (seq solutions) (:cost (apply min-key :cost solutions)) 0)
           :max-cost (if (seq solve-max) (:cost (apply min-key :cost solve-max)) 0))))

(defn part1 [in]
  (map solve in))

(defn score [games]
  (int (apply + (map :min-cost games))))

(assert (= 480 (score (part1 (parse example)))))
(assert (= 31589 (score (part1 (parse input)))))

(comment (filter (fn [{:keys [min-cost max-cost] {:keys [ma]} :max}]
                   (not= min-cost max-cost))
                 (part1 (parse input))))

(defn part2 [in]
  (->> in
       (map (fn [game] (update game :goal v/v+ [10000000000000 10000000000000])))
       (map solve)))

(assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
