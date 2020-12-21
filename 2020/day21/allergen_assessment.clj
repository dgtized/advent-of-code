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

(defn allergens-map [input]
  (apply merge-with set/union
         (for [[allergens ingredients] input
               allergy allergens]
           {allergy (set ingredients)})))

(defn ingredients-freq [input]
  (apply merge-with +
         (for [[_ ingredients] input]
           (frequencies ingredients))))

(defn ingredients-map [input]
  (apply merge-with set/union
         (for [[allergens ingredients] input
               ingredient ingredients]
           {ingredient (set allergens)})))

(defn probable-ingredients [input]
  (reduce (fn [known [allergens ingredients]]
            (reduce (fn [prove allergen]
                      (println [prove allergen])
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

(defn combined [filename]
  (let [input (parse filename)]
    [(map (juxt first (comp sort second)) input)
     (allergens-map input)
     (ingredients-map input)]))

(comment
  (allergens-map (parse "example"))
  (sort-by second (ingredients-freq (parse "example")))
  (combined "example")

  (probable-ingredients (parse "example"))
  (probable-ingredients (parse "input"))

  (= 5 (first-star "example"))
  (= 2786 (first-star "input"))
  )
