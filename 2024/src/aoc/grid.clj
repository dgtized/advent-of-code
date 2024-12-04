(ns aoc.grid)

(defn lines->grid [lines]
  (->> lines
       (mapcat (fn [j line]
                 (map-indexed (fn [i c] [[i j] c]) line))
               (range (count lines)))
       (into {})))
