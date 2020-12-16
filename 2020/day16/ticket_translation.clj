(ns ticket-translation
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse
  [filename]
  (let [contents (slurp filename)
        tickets (mapv (fn [t] (mapv edn/read-string (s/split (second t) #",")))
                      (re-seq #"(\d+,(\d+,?)+)\n" contents))]
    {:rules (into {}
                  (for [[_ field & ranges]
                        (re-seq #"([^:]+): (\d+)-(\d+) or (\d+)-(\d+)\n" contents)]
                    [field (partition 2 (map edn/read-string ranges))]))
     :ticket (first tickets) :tickets (rest tickets)}))

(defn between? [[lower upper]]
  (fn [x]
    (and (>= x lower) (<= x upper))))

(defn valid-ranges [{:keys [rules]}]
  (map between?
       (mapcat identity (vals rules))))

(defn invalid-fields
  [input]
  (let [valid-field? (apply some-fn (valid-ranges input))]
    (fn [ticket]
      (filter (complement valid-field?) ticket))))

(defn valid-ticket
  [input]
  (let [invalid (invalid-fields input)]
    (fn [ticket]
      (empty? (invalid ticket)))))

(defn scanning-error-rate [filename]
  (let [input (parse filename)]
    (->> (:tickets input)
         (map (invalid-fields input))
         flatten
         (apply +))))

(defn possible-rules [rules]
  (for [[field constraints] rules
        :let [rule-applies (apply some-fn (map between? constraints))]]
    (fn [n] (when (rule-applies n) field))))

(comment
  (not ((apply some-fn (map between? [[1 2] [4 5]])) 3))
  (= ((first (possible-rules {:r [[1 2] [3 4]]})) 1) :r))

(defn rule-match->fields
  [{:keys [rules]}]
  (let [possibilities (possible-rules rules)]
    (fn [n] (into #{} (keep (fn [r] (r n)) possibilities)))))

(comment
  (= ((rule-match->fields {:rules {:a [[1 2] [4 5]] :b [[0 2] [4 6]]}}) 4)
     #{:b :a}))

(defn transpose [m]
  (apply mapv vector m))

(comment
  (transpose [[1 2] [3 4]]))

(defn collapse-possibilities [columns]
  (if (every? string? columns)
    columns
    (let [singleton (some (fn [x] (when (= (count x) 1) x)) columns)
          element (first singleton)]
      (recur (for [x columns]
               (cond (= x singleton) element
                     (string? x) x
                     :else (disj x element)))))))

(comment
  (= (collapse-possibilities '(#{"row"} #{"class" "row"} #{"class" "row" "seat"}))
     ["row" "class" "seat"]))

(defn field-columns [filename]
  (let [input (parse filename)
        tickets (filter (valid-ticket input) (:tickets input))
        columns (transpose tickets)
        rule-matches (rule-match->fields input)
        candidate-columns (map (fn [column] (reduce set/intersection
                                                   (map rule-matches column)))
                               columns)]
    (zipmap (collapse-possibilities candidate-columns)
            (:ticket input))))

(defn product-of-departure-fields
  [field-values]
  (apply * (for [[field value] field-values
                 :when (re-find #"departure" field)]
             value)))

(comment
  (= (scanning-error-rate "example") 71)
  (= (scanning-error-rate "input") 24110)

  (= (field-columns "example2")
     {"row" 11, "class" 12, "seat" 13})
  (= (product-of-departure-fields (field-columns "input"))
     6766503490793))






