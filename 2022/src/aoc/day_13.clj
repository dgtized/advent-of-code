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

(defn check [l r]
  (cond (and (integer? l) (integer? r))
        (cond (< l r)
              true
              (> l r)
              false
              :else
              nil)
        (and (vector? l) (vector? r))
        (loop [left l right r]
          (let [lv (first left)
                rv (first right)]
            (cond (and (nil? lv) (not (nil? rv)))
                  true
                  (and (nil? rv) (not (nil? lv)))
                  false
                  (and (nil? rv) (nil? lv))
                  nil
                  :else
                  (let [result (check lv rv)]
                    (if-not (nil? result)
                      result
                      (recur (rest left) (rest right)))))))
        (and (integer? l) (vector? r))
        (check [l] r)
        (and (vector? l) (integer? r))
        (check l [r])))

(comment
  (check [1 1 3 1 1] [1 1 5 1 1])
  (check [[1] [2 3 4]] [[1] 4])
  (check [[4,4],4,4]
         [[4,4],4,4,4])
  (check [[[]]] [[]]))

(defn star1 [file]
  (keep (fn [[i v]] (when v i))
        (map-indexed (fn [i x] [(inc i) (apply check x)])
                     (parse file))))

(star1 "input/day13.example")

(defn star2 [file]
  [])

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day13")
 (fn [{:keys [result]}]
   [(apply + result) result]))
