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

(defn remove-zeros [coll]
  (remove (fn [v] (re-find #"0" (str v))) coll))

(comment
  (->> (range 99999999999999 11111111111111 -1000000)
       remove-zeros
       (filter valid?)
       (take 1)))

(comment
  (->> (range 11111111111111 11111111112111)
       remove-zeros
       (mapv (fn [n] [n (evaluate (num->digits n) monad)])))

  (->> (range 51111111111111 51111111111911)
       remove-zeros
       (mapv (fn [n] [n (evaluate (num->digits n) monad)])))

  (->> (range 99999999999999 99999999998999 -1)
       remove-zeros
       (mapv (fn [n] [n (evaluate (num->digits n) monad)])))

  (for [n (remove-zeros (range 11 100))]
    [n (evaluate (num->digits n) (take 36 monad))])

  (for [n (random-sample 0.001 (remove-zeros (range 111111 999999 1)))]
    [n (evaluate (num->digits n) (take (* 18 6) monad))])

  (for [n (range 1 10)
        part (partition 18 monad)]
    [n (evaluate (num->digits n) (take 6 part))])


  )

;; every digit group is 18 instructions with identical operations to registers
(assert (apply = (mapv (fn [digit] (map (partial take 2) digit)) (partition 18 monad))))

;; most of the final args are same but a few columns vary
(mapv (fn [digit] (map last digit)) (partition 18 monad))
