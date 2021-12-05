(ns hydrothermal-venture
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-lines [filename]
  (->> filename
       slurp
       str/split-lines
       (mapv (fn [line]
               (->> line
                    (re-matches #"(\d+),(\d+) -> (\d+),(\d+)")
                    rest
                    (mapv edn/read-string))))))

(defn ink-line [screen points]
  (reduce (fn [m p] (update m p (fnil inc 0)))
          screen
          points))

(defn line [[x0 y0 x1 y1]]
  (cond (= x0 x1)
        (mapv vector
              (repeatedly (constantly x0))
              (range (min y0 y1) (inc (max y0 y1))))
        (= y0 y1)
        (mapv vector
              (range (min x0 x1) (inc (max x0 x1)))
              (repeatedly (constantly y0)))
        :else []))

(defn intersections [line filename]
  (->> filename
       parse-lines
       (mapcat line)
       (ink-line {})
       vals
       (remove #(= 1 %))
       count))

(def part1 (partial intersections line))

(assert (= 5 (part1 "example")))
(assert (= 7380 (part1 "input")))

(defn diag-line [[x0 y0 x1 y1]]
  (let [x-range (range (min x0 x1) (inc (max x0 x1)))
        y-range (range (min y0 y1) (inc (max y0 y1)))]
    (cond (= x0 x1)
          (mapv vector
                (repeatedly (constantly x0))
                y-range)
          (= y0 y1)
          (mapv vector
                x-range
                (repeatedly (constantly y0)))
          :else
          (mapv vector
                (if (> x0 x1) (reverse x-range) x-range)
                (if (> y0 y1) (reverse y-range) y-range)))))

(comment (diag-line [0 9 5 9])
         (diag-line [8 0 0 8])
         (diag-line [1 1 3 3])
         (diag-line [9 7 7 9]))

(def part2 (partial intersections diag-line))

(assert (= 12 (part2 "example")))
(assert (= 21373 (part2 "input")))
