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

(defn winnow-ingredients
  "Reduces [[allergens ...] => [ingredients ...]] to {allergen #{ingredients ...} ...}

  For each allergen, iteratively winnows the list of ingredients by intersecting
  with each subsequent reference to the allergen."
  [input]
  (reduce (fn [known [allergens ingredients]]
            (reduce (fn [prove allergen]
                      (update prove allergen
                              (fn [v] (if (nil? v) (set ingredients)
                                         (set/intersection (set ingredients) v)))))
                    known
                    allergens))
          {}
          input))

(defn first-star
  "Removes the union of all likely ingredients to count ingredients remaining. "
  [filename]
  (let [input (parse filename)
        probables (->> input
                       winnow-ingredients
                       vals
                       (apply set/union))]
    (count (remove probables (mapcat second input)))))

(defn assign-allergens
  "Uniquely assign allergen -> ingredient

  Sorts probable ingredients so smallest set is first, assumes that is a single
  ingredient, assigns it as proven to be that allergen and then reduces each
  subsequent mapping by removing any proven ingredients."
  [probables]
  (reduce (fn [proven [allergen ingredients]]
            (assoc proven allergen
                   (first (apply disj ingredients
                                 (vals proven)))))
          {}
          (sort-by (comp count second) probables)))

(defn second-star
  [filename]
  (->> filename
       parse
       winnow-ingredients
       assign-allergens
       (sort-by first)
       (map second)
       (str/join ",")))

(comment
  (assign-allergens (winnow-ingredients (parse "example")))
  (assign-allergens (winnow-ingredients (parse "input")))

  (= 5 (first-star "example"))
  (= 2786 (first-star "input"))

  (= (second-star "example")
     "mxmxvkd,sqjhc,fvjkl")
  (= (second-star "input")
     "prxmdlz,ncjv,knprxg,lxjtns,vzzz,clg,cxfz,qdfpq")
  )
