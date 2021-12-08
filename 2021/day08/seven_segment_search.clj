(ns seven-segment-search
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map (fn [s] (map sort (str/split s #"\s+"))) (str/split line #"\|\s+")))

(defn parse [filename]
  (mapv parse-line (str/split-lines (slurp filename))))

;; Canonical representation:
;;  aaaa
;; b    c
;; b    c
;;  dddd
;; e    f
;; e    f
;;  gggg

(def known-digits
  {2 1 ;; cf
   3 7 ;; acf
   4 4 ;; bcdf
   7 8 ;; abcdefg
   })

;; 9 [abcdfg] 6 missing e
;; 6 [abdefg] 6 missing c
;; 5 [abdfg]  5 missing c,e
;; 3 [acdfg]  5 missing b,e
;; 2 [acdeg]  5 missing b,f
;; 0 [abcefg] 6 missing d

(defn part1 [input]
  (let [freqs (->> input
                   (map second)
                   (mapcat #(map (comp known-digits count) %))
                   frequencies)]
    (apply + (vals (dissoc freqs nil)))))

(assert (= 26 (part1 (parse "example"))))
(assert (= 352 (part1 (parse "input"))))

(def ex2 (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

(defn solve-digits [digits]
  (let [base (zipmap digits
                     (for [digit digits
                           :let [c (count digit)]]
                       (set (if-let [k (known-digits c)]
                              [k]
                              (if (= c 5)
                                [2 3 5]
                                [0 6 9])))))]
    base))

(comment (solve-digits (first ex2)))
