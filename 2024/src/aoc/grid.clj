(ns aoc.grid
  (:require [clojure.string :as str]))

(defn grid->str [grid]
  (let [ks (keys grid)
        w (first (apply max-key first ks))
        h (second (apply max-key second ks))]
    (str/join "\n"
              (for [y (range h)]
                (apply str
                       (for [x (range w)]
                         (get grid [x y])))))))

(defn lines->grid [lines]
  (->> lines
       (mapcat (fn [j line]
                 (map-indexed (fn [i c] [[i j] c]) line))
               (range (count lines)))
       (into {})))

(defn some-value [grid value]
  (some (fn [[pos v]] (when (= value v) pos)) grid))

(defn keep-values [grid value]
  (keep (fn [[pos v]] (when (= value v) pos)) grid))

(defn dims [grid]
  (let [ks (keys grid)]
    [(inc (first (apply max-key first ks)))
     (inc (second (apply max-key second ks)))]))
