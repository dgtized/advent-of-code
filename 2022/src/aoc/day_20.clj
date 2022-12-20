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

(defn bias [idx len n]
  (cond (and (neg? n) (<= (+ idx n) 0))
        -1
        (and (pos? n) (> (+ idx n) len))
        1
        :else 0))

(defn mix [input]
  (let [len (count input)]
    (reduce
     (fn [state n]
       (let [state' (remove #{n} state)]
         (if (zero? n)
           state
           (let [idx (.indexOf state n)
                 pos (mod (+ idx n (bias idx len n)) len)
                 [before after] (split-at pos state')]
             (concat before [n] after)))))
     input
     input)))

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
