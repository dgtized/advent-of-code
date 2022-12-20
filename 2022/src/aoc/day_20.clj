(ns aoc.day-20
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 20 :name "example" :content "")
#_(aoc/day-input :day 20)

(defn parse [file]
  (map parse-long (aoc/file->lines file)))

(def example (parse "input/day20.example"))
(def input (parse "input/day20.input"))

(defn move [len]
  (fn [state [i n]]
    (if (zero? n)
      state
      (let [idx (.indexOf state [i n])
            state' (into [] (concat (take idx state) (drop (inc idx) state)))
            pos (mod (+ idx n) len)
            [before after] (split-at (if (zero? pos) len pos) state')]
        (concat before [[i n]] after)))))

(defn mix [input n]
  (let [len (dec (count input))
        indexed (map-indexed vector input)
        f (fn [xs] (reduce (move len) xs indexed))]
    (map second (first (drop n (iterate f indexed))))))

(def expected [[1 2 -3 3 -2 0 4]
               [2 1 -3 3 -2 0 4]
               [1 -3 2 3 -2 0 4]
               [1 2 3 -2 -3 0 4]
               [1 2 -3 0 3 4 -2]
               [1 2 -3 0 3 4 -2]
               [1 2 -3 4 0 3 -2]])

#_(mix example 1)

(defn coordinates [input]
  (let [len (count input)
        idx (.indexOf input 0)]
    (map (fn [i] (nth input (mod (+ idx i) len)))
         [1000 2000 3000])))

(defn star1 [file]
  (let [c (coordinates (mix (parse file) 1))]
    [(apply + c) c]))

(defn star2 [file]
  (let [c (coordinates (mix (map (fn [n] (* n 811589153)) (parse file)) 10))]
    [(apply + c) c]))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day20")
 (fn [{:keys [result]}] result))
