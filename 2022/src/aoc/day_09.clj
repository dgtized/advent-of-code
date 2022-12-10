(ns aoc.day-09
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 9 :name "example" :content "")
#_(aoc/day-input :day 9)

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))

(defn touching? [[x0 y0] [x1 y1]]
  (and (<= (Math/abs (- x1 x0)) 1)
       (<= (Math/abs (- y1 y0)) 1)))

#_(touching? [0 0] [1 0])
#_(touching? [0 0] [2 0])

(def dirs {"R" [1 0]
           "L" [-1 0]
           "U" [0 1]
           "D" [0 -1]})

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (let [[d c] (str/split line #"\s")]
      [(get dirs d) (parse-long c)])))

(defn head-path [input]
  (reductions v+
              [0 0]
              (mapcat (fn [[dir n]] (repeat n dir)) input)))

(defn constrain [v]
  (cond (>= v 1)
        1
        (<= v -1)
        -1
        :else 0))

(defn delta [pos head]
  (mapv constrain (v- head pos)))

#_(delta [1 1] [3 1])
#_(delta [1 1] [3 2])
#_(delta [2 4] [4 3])

(defn follow [path]
  (reductions (fn [pos head]
                (if (touching? pos head)
                  pos
                  (v+ pos (delta pos head))))
              path))

(defn pair-path [file]
  (let [path (head-path (parse file))]
    (map vector (follow path) path)))

#_(pair-path "input/day09.example")

(defn star1 [file]
  (let [path (follow (head-path (parse file)))]
    [(count (set path))]))

(defn star2 [file]
  (let [path (head-path (parse file))
        tail (last (take 10 (iterate follow path)))]
    [(count (set tail))]))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day09")
 (fn [{:keys [result]}] result))

(defn path->grid [path]
  (let [cols (+ 2 (apply max (map first path)))
        rows (+ 2 (apply max (map second path)))]
    (into []
          (for [j (reverse (range rows))]
            (into []
                  (for [i (range cols)]
                    (if (contains? path [i j])
                      1
                      0)))))))

(clerk/with-viewers
  [{:pred integer?
    :render-fn '(fn [n] [:div.inline-block
                        {:style {:width 20 :height 20}
                         :class (if (pos? n)
                                  "bg-black"
                                  "bg-white border-grey border-solid border-2")}])}
   {:pred (fn [x] (and (vector? x) (every? vector? x)))
    :render-fn '(fn [rows opts] (into [:div.flex.flex-col] (v/inspect-children opts) rows))}
   {:pred (fn [x] (and (vector? x) (every? int? x)))
    :render-fn '(fn [row opts] (into [:div.flex.inline-flex] (v/inspect-children opts) row))}]
  (path->grid (set (follow (head-path (parse "input/day09.example"))))))
