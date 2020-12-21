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

(defn assign-allergens [probables]
  (reduce (fn [proven [allergen ingredients]]
            (assoc proven allergen
                   (first (apply disj ingredients
                                 (flatten (vals proven))))))
          {}
          (sort-by (comp count second) probables)))

(defn second-star [filename]
  (->> filename
       parse
       probable-ingredients
       assign-allergens
       (sort-by first)
       (map second)
       (str/join ",")))

(comment
  (assign-allergens (probable-ingredients (parse "example")))
  (assign-allergens (probable-ingredients (parse "input")))

  (= 5 (first-star "example"))
  (= 2786 (first-star "input"))

  (= (second-star "example")
     "mxmxvkd,sqjhc,fvjkl")
  (= (second-star "input")
     "prxmdlz,ncjv,knprxg,lxjtns,vzzz,clg,cxfz,qdfpq")
  )
