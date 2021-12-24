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

(defn interpret [{:keys [input] :as state} instr]
  (match [instr]
    [[:inp a]] (assoc state a (first input)
                      :input (rest input))
    [[:add a b]] (assoc state a (+ (value state a) (value state b)))
    [[:mul a b]] (assoc state a (* (value state a) (value state b)))
    [[:div a b]] (assoc state a (int (/ (value state a) (value state b))))
    [[:mod a b]] (assoc state a (mod (value state a) (value state b)))
    [[:eql a b]] (assoc state a (if (= (value state a) (value state b)) 1 0))))

(defn evaluate [input program]
  (reduce interpret (assoc (init) :input input) program))

;; (reduce interpret (init) (parse "example"))

(assert (= {:w 0, :x -1, :y 0, :z 0, :input []}
           (evaluate [1] (parse "example"))))

(defn num->digits [n]
  (let [s (str n)]
    (into []
          (for [x (range (count s))]
            (edn/read-string (subs s x (inc x)))))))

(assert (= [1 2 3 4] (num->digits 1234)))

(assert (= {:w 0, :x 0, :y 1, :z 1 :input []}
           (evaluate (num->digits 3) (parse "example2"))))

(def monad (parse "input"))

(defn valid? [n]
  (let [{:keys [z]} (evaluate (num->digits n) monad)]
    (if (= z 0)
      true
      false)))

(assert (not (valid? 13579246899999)))
(assert (not (valid? 31111111111115)))
(assert (not (valid? 99999999999999)))

(comment
  (->> (range 99999999999999 11111111111111 -1)
       (remove (fn [v] (re-find #"0" (str v))))
       (filter valid?)
       (take 1)))
