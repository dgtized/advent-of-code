(ns dumbo-octopus
  (:require [clojure.string :as str]))

(defn parse [filename]
  (mapv (fn [line] (mapv (comp parse-long str) (seq line)))
        (str/split-lines (slurp filename))))

(defn grid->map [grid]
  (into {}
        (for [j (range 10)
              i (range 10)]
          {[i j] (nth (nth grid j) i)})))

(defn map->grid [map]
  (->> (for [j (range 10)
             i (range 10)]
         (get map [i j]))
       (partition 10)
       (mapv vec)))

(defn step [grid]
  (let [m (grid->map grid)]
    (map->grid (update-vals m inc))))

(comment (map->grid (grid->map (parse "example"))))

(step (parse "example"))

