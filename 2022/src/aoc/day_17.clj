(ns aoc.day-17
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 17 :name "example" :content "")
#_(aoc/day-input :day 17)

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))

(defn height [grid]
  (if (empty? grid)
    0
    (inc (apply max (map second (keys grid))))))

(defn rock-height [rock]
  (inc (apply max (map second rock))))

(defn parse-rocks [s]
  (for [rock-lines (aoc/split-empty-lines s)]
    (for [[y line] (map-indexed vector (reverse (str/split-lines rock-lines)))
          [x c] (map-indexed vector line)
          :when (= c \#)]
      [x y])))

(def rocks (parse-rocks "####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"))

(defn parse [file]
  (for [dir (seq (str/trim (slurp file)))]
    (get {\< [-1 0] \> [1 0]} dir)))

(def example (parse "input/day17.example"))
(def input (parse "input/day17.input"))

(mapv rock-height rocks)

(defn show-grid [grid]
  (let [h (height grid)]
    (for [y (reverse (range 0 h))]
      (mapv #(get grid [% y] ".") (range 7)))))

(defn collision? [grid rock]
  (some (fn [[x y :as coord]] (or (get grid coord nil)
                                 (< x 0)
                                 (> x 6)
                                 (< y 0)))
        rock))

(defn update-rock [grid rock dir]
  (let [rock' (map (partial v+ dir) rock)]
    (if (collision? grid rock')
      rock
      rock')))

(defn place-rock [grid rock]
  (reduce (fn [g r] (assoc g r "#")) grid rock))

(defn rock-fall [grid shape jets]
  (let [start [2 (+ (height grid) 3)]
        initial (map (partial v+ start) shape)]
    ;; (println initial "init")
    (loop [rock initial directions (interpose [0 -1] jets) steps 0]
      (let [dir (first directions)
            rock' (update-rock grid rock dir)]
        ;; (println dir rock')
        ;; (doseq [line (show-grid (place-rock grid rock'))]
        ;;   (println line))
        (if (and (= rock rock') (= dir [0 -1]))
          [(/ (inc steps) 2) (place-rock grid rock)]
          (recur rock' (rest directions) (inc steps)))))))

(defn drop-rocks [jets n]
  ;; (println)
  (loop [grid {} rocks (cycle rocks) jets (cycle jets) n n]
    (if (zero? n)
      grid
      (let [[steps grid'] (rock-fall grid (first rocks) jets)]
        (recur grid' (rest rocks) (drop steps jets) (dec n))))))

(comment
  (show-grid (second (rock-fall {} (first rocks) example)))
  (show-grid (second (rock-fall {} (nth rocks 2) example)))
  (show-grid (place-rock {} (nth rocks 2)))
  (show-grid (drop-rocks example 10)))

(defn star1 [file]
  (height (drop-rocks (parse file) 2022)))

(comment
  (map (fn [n]
         (let [v (height (drop-rocks example n))
               p (height (drop-rocks example (dec n)))]
           [n v (mod n 5) (mod n (count example)) (- v p)])) (range 1 300))

  (- 125 66)
  (- 184 125)

  (* (count rocks) (count example))
  (* (count rocks) (count input)))

(defn match-cycle [deltas base len]
  (when (every? (fn [i] (= (nth deltas (+ base i))
                          (nth deltas (+ base len i) nil)
                          (nth deltas (+ base (* 2 len) i) nil)))
                (range len))
    (let [cycles 1000000000000
          height (reduce + (subvec deltas 0 base))
          delta (reduce + (subvec deltas base (+ base len)))]
      {:base base
       :length len
       :height height
       :delta delta
       :total (+ height (* delta (quot (- cycles base) len))
                 (reduce + (subvec deltas base (+ base (rem (- cycles base) len)))))})))

(defn rock-cycles [jets n]
  (loop [grid {} rocks (cycle rocks) jets (cycle jets) i 0 deltas []]
    (if (< i n)
      (let [[steps grid'] (rock-fall grid (first rocks) jets)
            h (height grid')]
        (recur grid' (rest rocks) (drop steps jets) (inc i)
               (conj deltas (- h (height grid)))))
      (some (fn [base]
              (some (fn [len] (match-cycle deltas base len))
                    (range 20 (/ (count deltas) 4))))
            (range 0 (/ (count deltas) 2))))))

#_(time (rock-cycles example 300))
#_(time (rock-cycles input 10000))
;; "Elapsed time: 148359.660382 msecs"
;; {:base 532, :length 1725, :height 800, :delta 2630, :total 1524637681145}

(defn star2 [file]
  (rock-cycles (parse file) 5000))

#_(star2 "input/day17.example")
#_(star2 "input/day17.input")

{::clerk/visibility {:result :show}}
#_(aoc/answer-table
   [star1 star2]
   (aoc/input-files "day17")
   (fn [{:keys [result]}] result))
