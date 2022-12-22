(ns aoc.day-22
  (:require
   [aoc.utility :as aoc]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 22 :name "example" :content "")
#_(aoc/day-input :day 22)

(defn parse-grid [grid]
  (into {}
        (for [[j line] (map-indexed vector (str/split-lines grid))
              [i c] (map-indexed vector (seq line))
              :when (contains? #{\# \.} c)]
          [[i j] c])))

(defn parse [file]
  (let [[grid dirs] (aoc/split-empty-lines (slurp file))]
    {:grid (parse-grid grid)
     :path (->> dirs
                (re-seq #"([RL]|\d+)")
                (mapv (fn [[_ m]] (or (parse-long m) m))))}))

(def example (parse "input/day22.example"))
(def input (parse "input/day22.input"))

(defn simplify-extent [extent]
  (->> extent
       (partition-by second)
       (map (fn [group] [[(ffirst group) (first (last group))] (second (first group))]))
       (into {})))

(defn extents [{:keys [grid]}]
  (let [max-y (apply max (map second (keys grid)))
        max-x (apply max (map first (keys grid)))]
    [(simplify-extent
      (for [i (range 0 (inc max-x))
            :let [column (keep (fn [j] (when (grid [i j]) [i j])) (range 0 (inc max-y)))]]
        [i [(second (first column)) (second (last column))]]))
     (simplify-extent
      (for [j (range 0 (inc max-y))
            :let [row (keep (fn [i] (when (grid [i j]) [i j])) (range 0 (inc max-x)))]]
        [j [(ffirst row) (first (last row))]]))]))

(extents example)

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day22")
 (fn [{:keys [result]}] result))
