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

(defn line [screen [x0 y0 x1 y1]]
  (cond (= x0 x1)
        (reduce (fn [m y] (update m [x0 y] (fnil inc 0)))
                screen
                (range (min y0 y1) (inc (max y0 y1))))
        (= y0 y1)
        (reduce (fn [m x] (update m [x y0] (fnil inc 0)))
                screen
                (range (min x0 x1) (inc (max x0 x1))))
        :else
        screen))

(defn part1 [filename]
  (->> filename
       parse-lines
       (reduce line {})
       vals
       (remove #(= 1 %))
       count))

(assert (= 5 (part1 "example")))
(assert (= 7380 (part1 "input")))

(defn diag-line [screen [x0 y0 x1 y1]]
  (let [x-range (range (min x0 x1) (inc (max x0 x1)))
        y-range (range (min y0 y1) (inc (max y0 y1)))]
    (reduce (fn [m p] (update m p (fnil inc 0)))
            screen
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
                        (if (> y0 y1) (reverse y-range) y-range))))))

(comment (diag-line {} [0 9 5 9])
         (diag-line {} [8 0 0 8])
         (diag-line {} [1 1 3 3])
         (diag-line {} [9 7 7 9]))

(defn part2 [filename]
  (->> filename
       parse-lines
       (reduce diag-line {})
       vals
       (remove #(= 1 %))
       count))

(assert (= 12 (part2 "example")))
(assert (= 21373 (part2 "input")))



