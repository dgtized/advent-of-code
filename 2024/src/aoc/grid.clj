(ns aoc.grid)

(defn lines->grid [lines]
  (->> lines
       (mapcat (fn [j line]
                 (map-indexed (fn [i c] [[i j] c]) line))
               (range (count lines)))
       (into {})))

(defn some-value [grid value]
  (some (fn [[pos v]] (when (= value v) pos)) grid))
