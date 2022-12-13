(ns aoc.day-13
  (:require
   [aoc.utility :as aoc]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 13 :name "example" :content "")
#_(aoc/day-input :day 13)

(defn parse [file]
  (for [group (aoc/split-empty-lines (slurp file))]
    (map edn/read-string (str/split-lines group))))

#_(parse "input/day13.example")
#_(parse "input/day13.input")

(defn kind [x]
  (cond (integer? x) :int
        (vector? x) :vec))

(defn check [l r]
  (condp = [(kind l) (kind r)]
    [:int :int]
    (cond (< l r)
          true
          (> l r)
          false
          :else
          nil)
    [:vec :vec]
    (loop [left l right r]
      (let [lv (first left)
            rv (first right)]
        (condp = [(nil? lv) (nil? rv)]
          [true false] true
          [false true] false
          [true true] nil
          (let [result (check lv rv)]
            (if-not (nil? result)
              result
              (recur (rest left) (rest right)))))))
    [:int :vec]
    (check [l] r)
    [:vec :int]
    (check l [r])))

(comment
  (check [1 1 3 1 1] [1 1 5 1 1])
  (check [[1] [2 3 4]] [[1] 4])
  (check [[4,4],4,4]
         [[4,4],4,4,4])
  (check [[[]]] [[]]))

(defn star1 [file]
  (->> file
       parse
       (map-indexed (fn [i x] [(inc i) (apply check x)]))
       (keep (fn [[i v]] (when v i)))
       (apply +)))

(star1 "input/day13.example")

(defn star2 [file]
  (let [dividers [[[2]] [[6]]]]
    (->> (parse file)
         (mapcat identity)
         (concat dividers)
         (sort (fn [a b]
                 (let [r (check a b)]
                   (case r
                     true -1
                     false 1
                     nil 0))))
         (map-indexed (fn [i v] [(inc i) v]))
         (keep (fn [[i v]] (when (contains? (set dividers) v) i)))
         (apply *))))

(star2 "input/day13.example")
(star2 "input/day13.input") ;; 26289

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day13")
 (fn [{:keys [result]}] result))
