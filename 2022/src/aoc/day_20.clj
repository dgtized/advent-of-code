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

(defn mix [input]
  (let [len (dec (count input))]
    (map second
         (reduce
          (fn [state [i n]]
            (if (zero? n)
              state
              (let [idx (.indexOf state [i n])
                    state' (into [] (concat (take idx state) (drop (inc idx) state)))
                    pos (mod (+ idx n) len)
                    [before after] (split-at (if (zero? pos) len pos) state')]
                (concat before [[i n]] after))))
          (map-indexed vector input)
          (map-indexed vector input)))))

(def expected [[1 2 -3 3 -2 0 4]
               [2 1 -3 3 -2 0 4]
               [1 -3 2 3 -2 0 4]
               [1 2 3 -2 -3 0 4]
               [1 2 -3 0 3 4 -2]
               [1 2 -3 0 3 4 -2]
               [1 2 -3 4 0 3 -2]])

#_(mix example)

(defn coordinates [input]
  (let [len (count input)
        idx (.indexOf input 0)]
    (map (fn [i] (nth input (mod (+ idx i) len)))
         [1000 2000 3000])))

(defn star1 [file]
  (let [c (coordinates (mix (parse file)))]
    [(apply + c) c]))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day20")
 (fn [{:keys [result]}] result))
