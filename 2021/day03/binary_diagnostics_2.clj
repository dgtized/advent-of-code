(ns binary_diagnostics_2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example (str/split-lines (slurp "example")))
(def input (str/split-lines (slurp "input")))

(defn most-common [input pos]
  (let [freqs (frequencies (mapv #(nth % pos) input))
        on (get freqs \1)
        off (get freqs \0)]
    (if (>= on off)
      \1
      \0)))

(defn least-common [input pos]
  (get {\1 \0 \0 \1} (most-common input pos)))

(defn winnow [common input]
  (reduce (fn [xs pos]
            (if (= 1 (count xs))
              (reduced xs)
              (let [bit (common xs pos)]
                (filter #(= (nth % pos) bit) xs))))
          input (range (count (first example)))))

(defn oxygen [input]
  (first (winnow most-common input)))

(defn co2-scrubber [input]
  (first (winnow least-common input)))

(comment
  (oxygen example)
  (co2-scrubber example))

(defn binary [s]
  (let [l (count s)]
    (int (reduce (fn [acc p] (if (= \1 (nth s p))
                              (+ acc (Math/pow 2 (- l p 1)))
                              acc))
                 0
                 (range l)))))

(defn second-star [input]
  (let [o (oxygen input)
        co2 (co2-scrubber input)
        [a b] (mapv binary [o co2])]
    [o a co2 b (* a b)]))

(second-star example)
(second-star input)



