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

(defn eql [a b]
  (if (= a b) 1 0))

;; find all digit/pairs where z will be zeroed
(for [a [15 15 12 13 -12 10 -9 14 13 -14 -11 -2 -16 -14]]
  [a (for [z (range 0 26)
           digit (range 1 10)
           :when (= 0 (eql 0 (eql digit (+ (mod z 26) a))))]
       [z digit])])

;; possible solutions for first 4 leading to a 0 on block 5
(for [n (remove-zeros (range 1111 (inc 9999)))
      :let [{:keys [z]} (evaluate (num->digits n) (take (* 18 4) monad))]
      :when (#{13 14 15 16 17 18 19 20 21} (mod z 26) )]
  [n z])

;; 8995
(def first-half
  (doall (for [n (remove-zeros (range 7000 (inc 8995)))
               :let [{:keys [z]} (evaluate (num->digits n) (take (* 18 4) monad))]
               :when ((set (range 13 22)) (mod z 26) )
               d1 (range 1 10) #_[9]
               :let [n2 (edn/read-string (str n d1))
                     {z2 :z} (evaluate (num->digits n2) (take (* 18 5) monad))]
               ;; :when (<= (count (str z2)) 5)
               d2 (range 1 10)
               :let [n3 (edn/read-string (str n2 d2))
                     {z3 :z} (evaluate (num->digits n3) (take (* 18 6) monad))]
               :when ((set (range 10 19)) (mod z3 26))
               d3 (range 1 10)
               :let [n4 (edn/read-string (str n3 d3))
                     {z4 :z} (evaluate (num->digits n4) (take (* 18 7) monad))]
               :when (<= (count (str z4)) 5)
               ]
           n4)))

(count first-half)

(def first-half2
  (doall (for [n (remove-zeros (range 600 (inc 899)))
               d1 [15 26 37 48 59]
               d2 [13 24 35 46 57]
               :let [prefix (edn/read-string (str n d1 d2))
                     {z :z} (evaluate (num->digits prefix) (take (* 18 7) monad))]
               :when (<= (count (str z)) 5)]
           prefix)))

(count first-half2)

;;   89959791916939
;; > 89959791916939
;;   89959794919939

;; < 89959131916939
;; < 89915131916939
(apply min-key first (for [n4 first-half2
                           d4 [19 29 39 49] #_(remove-zeros (range 11 100)) 
                           :let [n5 (edn/read-string (str n4 d4))
                                 {z5 :z} (evaluate (num->digits n5) (take (* 18 9) monad))]
                           :when ((set (range 15 24)) (mod z5 26))
                           d5 [1] #_(range 1 10)
                           :let [n6 (edn/read-string (str n5 d5))
                                 {z6 :z} (evaluate (num->digits n6) (take (* 18 10) monad))]
                           :when ((set (range 12 21)) (mod z6 26))
                           d6 [6 7 8 9] #_(range 1 10)
                           :let [n7 (edn/read-string (str n6 d6))
                                 {z7 :z} (evaluate (num->digits n7) (take (* 18 11) monad))]
                           :when (and ((set (range 3 12)) (mod z7 26)))
                           d7 [9] #_(range 1 10)
                           :let [n8 (edn/read-string (str n7 d7))
                                 {z8 :z} (evaluate (num->digits n8) (take (* 18 12) monad))]
                           :when (and ((set (range 17 26)) (mod z8 26))
                                      #_(= z8 617))
                           d8 [3] #_(range 1 10)
                           :let [n9 (edn/read-string (str n8 d8))
                                 {z9 :z} (evaluate (num->digits n9) (take (* 18 13) monad))]
                           :when (and #_(= z9 23)
                                      ((set (range 15 24)) (mod z9 26)))
                           d9 [9] #_(range 1 10)
                           :let [n10 (edn/read-string (str n9 d9))
                                 {z10 :z} (evaluate (num->digits n10) monad)]
                           :when (= z10 0)]
                       [n10 [d4 z5] [d5 z6] [d6 z7] [d7 z8] [d8 z9] [d9 z10]]))


