(ns alu
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defn parse [filename]
  (for [line (str/split-lines (slurp filename))
        :let [parts (str/split line #"\s+")]]
    (mapv (fn [v] (if (re-find #"\d+" v)
                   (edn/read-string v)
                   (keyword v)))
          parts)))

(parse "example")

(defn init []
  {:w 0
   :x 0
   :y 0
   :z 0})

(defn value [state v]
  (if (number? v)
    v
    (get state v)))

(defn interpret [state instr]
  (match [instr]
    [[:inp a]] (assoc state a (read))
    [[:add a b]] (assoc state a (+ (value state a) (value state b)))
    [[:mul a b]] (assoc state a (* (value state a) (value state b)))
    [[:div a b]] (assoc state a (int (/ (value state a) (value state b))))
    [[:mod a b]] (assoc state a (mod (value state a) (value state b)))
    [[:eql a b]] (assoc state a (if (= (value state a) (value state b)) 1 0))))

(defn evaluate [program]
  (reduce interpret (init) program))

;; (reduce interpret (init) (parse "example"))

(evaluate )
