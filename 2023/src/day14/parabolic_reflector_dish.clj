(ns day14.parabolic-reflector-dish
  (:require [clojure.string :as str]))

(def input (slurp "src/day14/input"))
(def example (slurp "src/day14/example"))

(defn parse [in]
  (str/split-lines in))

(defn rotate [m]
  (reverse (apply mapv str m)))

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
  (apply + (flatten (map (comp score slide) (rotate in)))))

(assert (= 136 (part1 (parse example))))
(assert (= 106517 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
