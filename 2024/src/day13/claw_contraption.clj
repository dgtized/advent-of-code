(ns day13.claw-contraption
  (:require
   [aoc.vector :as v]
   [clojure.string :as str]))

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

(defn solve-b [{:keys [a b goal]} push-a]
  (let [goal' (v/v- goal (v/v* a push-a))
        bx (/ (first goal') (first b))
        by (/ (second goal') (second b))]
    (when (= bx by)
      {:a push-a
       :b bx
       :cost (+ (* push-a 3) bx)})))

(defn search [{:keys [max] :as game}]
  (keep identity
        (for [push-a (range 0 (inc (:ma max)))]
          (solve-b game push-a))))

(defn solve [game]
  (let [game' (calc-max game)
        solutions (search game')]
    (assoc game'
           :solutions solutions
           :min-cost (if (seq solutions) (:cost (apply min-key :cost solutions)) 0))))

(defn part1 [in]
  (map solve in))

(defn score [games]
  (int (apply + (map :min-cost games))))

(assert (= 480 (score (part1 (parse example)))))
(assert (= 31589 (score (part1 (parse input)))))

(comment (filter (fn [{:keys [min-cost] {:keys [ma]} :max}]
                   (or (zero? min-cost)
                       (> ma 100)))
                 (part1 (parse input))))

(defn part2 [in]
  (->> in
       (map (fn [game] (update game :goal v/v+ [10000000000000 10000000000000])))
       (map solve)))

(assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
