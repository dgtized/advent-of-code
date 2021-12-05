(ns hydrothermal-venture
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse-lines [filename]
  (->> filename
       slurp
       str/split-lines
       (mapv (fn [line] (map edn/read-string (rest (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)))))))

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

(defn line2 [screen [x0 y0 x1 y1]]
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
                        (range (min x0 x1) (inc (max x0 x1)))
                        x-range)
                  :else
                  (mapv vector
                        (if (> x0 x1) (reverse x-range) x-range)
                        (if (> y0 y1) (reverse y-range) y-range))))))

(comment (line2 {} [8 0 0 8])
         (line2 {} [1 1 3 3])
         (line2 {} [9 7 7 9]))

(defn part2 [filename]
  (->> filename
       parse-lines
       (reduce line2 {})
       ;; vals
       ;; (remove #(= 1 %))
       ;; count

       ))

;; (assert (= 12 (part2 "example")))
;; (assert (= 0 (part2 "input")))



