(ns ticket-translation
  (:require [clojure.edn :as edn]
            [clojure.string :as s]))

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

(defn between? [lower upper]
  (fn [x]
    (and (>= x lower) (<= x upper))))

(defn valid-ranges [{:keys [rules]}]
  (map (fn [[l u]] (between? l u))
       (mapcat identity (vals rules))))

(defn invalid-field?
  [input]
  (fn [ticket]
    (filter (complement (apply some-fn (valid-ranges input)))
            ticket)))

(defn scanning-error-rate [filename]
  (let [input (parse filename)]
    (->> (:tickets input)
         (map (invalid-field? input))
         flatten
         (apply +))))

(comment
  (= (scanning-error-rate "example") 71)
  (= (scanning-error-rate "input") 24110))






