(ns allergen-assessment
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [filename]
  (for [line (-> filename slurp str/split-lines)
        :let
        [[_ ingredients allergens]
         (re-matches #"(.*) \(contains (.*)\)" line)]]
    [(str/split allergens #", ")
     (str/split ingredients #" ")]))

(defn probable-ingredients [input]
  (reduce (fn [known [allergens ingredients]]
            (reduce (fn [prove allergen]
                      (update prove allergen
                              (fn [v] (if (nil? v) (set ingredients)
                                         (set/intersection (set ingredients) v)))))
                    known
                    allergens))
          {}
          input))

(defn first-star [filename]
  (let [input (parse filename)
        probables (->> input
                       probable-ingredients
                       vals
                       (apply set/union))]
    (count (remove probables (mapcat second input)))))

(comment
  (probable-ingredients (parse "example"))
  (probable-ingredients (parse "input"))

  (= 5 (first-star "example"))
  (= 2786 (first-star "input"))
  )
