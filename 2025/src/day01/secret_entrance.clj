(ns day01.secret-entrance)

(def input (slurp "src/day01/input"))
(def example (slurp "src/day01/example"))

(defn parse [in]
  (for [[_ dir num] (re-seq #"([LR])(\d+)" in)]
    (* (if (= dir "L") -1 1) (parse-long num))))

(defn part1 [rotations]
  (count (filter #(= % 0) (reductions (fn [pos move] (mod (+ pos move) 100)) 50 rotations))))

(assert (= 3 (part1 (parse example))))
(assert (= 1182 (part1 (parse input))))

(defn part2 [in]
  (reductions
   (fn [[pos _ zeroes] move]
     (let [pos' (mod (+ pos move) 100)
           [_ _ z]
           (last (take-while
                  (fn [[p m _]] (not= p pos') (not (zero? m)))
                  (iterate
                   (fn [[p m z]]
                     (if (< m 0)
                       (let [p' (mod (+ p -1) 100)]
                         [p' (inc m) (+ z (if (zero? p') 1 0))])
                       (let [p' (mod (+ p 1) 100)]
                         [p' (dec m) (+ z (if (zero? p') 1 0))])))
                   [pos move 0])))]
       [pos' move (+ zeroes z (if (zero? pos') 1 0))]))
   [50 0 0] in))

(assert (= 6 (last (last (part2 (parse example))))))
(assert (= 6907 (last (last (part2 (parse input))))))
