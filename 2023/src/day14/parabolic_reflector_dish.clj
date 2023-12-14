(ns day14.parabolic-reflector-dish
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "src/day14/input"))
(def example (slurp "src/day14/example"))

(defn parse [in]
  (str/split-lines in))

(defn rotate-cw [m]
  (apply mapv str (reverse m)))

(defn rotate-ccw [m]
  (reverse (apply mapv str m)))

(rotate-cw ["ab" "cd"])
(rotate-ccw ["ab" "cd"])


(defn slide-chunk [chunk]
  (let [fr (frequencies chunk)]
    (str (apply str (repeat (get fr \O 0) "O"))
         (apply str (repeat (get fr \. 0) ".")))))

(defn slide [row]
  (let [block (str/index-of row "#")]
    (if block
      (str (slide-chunk (subs row 0 block))
           "#"
           (slide (subs row (inc block))))
      (slide-chunk row))))

(slide ".OO.O.#.O")

(defn score [row]
  (let [m (count row)]
    (keep-indexed (fn [i c] (when (= c \O)
                             (* (- m i)))) row)))

(defn part1 [in]
  (apply + (flatten (map (comp score slide) (rotate-ccw in)))))

(assert (= 136 (part1 (parse example))))
(assert (= 106517 (part1 (parse input))))

(defn slide-all [m]
  (->> m
       rotate-ccw
       (mapv slide)
       rotate-cw))

(rotate-cw (rotate-cw [".NN."
                       "W..E"
                       "W..E"
                       ".SS."]))

(defn spin-cycle [m]
  (->> m
       slide-all ;; north
       rotate-cw
       slide-all ;; west
       rotate-cw
       slide-all ;; south
       rotate-cw
       slide-all ;; east
       rotate-cw
       ))

(defn spins [in]
  (iterate spin-cycle in))

(nth (spins (parse example)) 1)
(nth (spins (parse example)) 2)
(nth (spins (parse example)) 3)

(defn value [m]
  (apply + (flatten (map score (rotate-ccw m)))))

;; (map-indexed vector (map value (take 200 (spins (parse example)))))
;; (map-indexed vector (map value (take 200 (spins (parse input)))))

(defn unique-cycle [m]
  (loop [seen {m 0} n 0 m m]
    (let [m' (spin-cycle m)]
      (if-let [start (get seen m')]
        [start (inc n) (mapv (comp value (set/map-invert seen)) (range start (inc n)))]
        (recur (assoc seen m' (inc n))
               (inc n)
               m')))))

(unique-cycle (parse example))
(unique-cycle (parse input))

(defn part2 [in]
  (let [[_ s1 scores] (unique-cycle in)]
    (nth scores (mod (- 1000000000 s1) (count scores)))))

(assert (= 64 (part2 (parse example))))
;; 79716 too low
(assert (= 79723 (part2 (parse input))))
