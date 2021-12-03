(ns binary_diagnostics_2
  (:require [clojure.string :as str]))

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
          input (range (count (first input)))))

(defn oxygen [input]
  (first (winnow most-common input)))

(defn co2-scrubber [input]
  (first (winnow least-common input)))

(defn second-star [input]
  (let [o (oxygen input)
        co2 (co2-scrubber input)
        [a b] (mapv #(Integer/parseInt % 2) [o co2])]
    [o a co2 b (* a b)]))

(assert (= ["10111" 23 "01010" 10 230]
           (second-star example)))
(assert (= ["000111111101" 509 "101010000101" 2693 1370737]
           (second-star input)))



