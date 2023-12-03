(ns day03.gear-ratios
  (:require [clojure.string :as str]))

(def input (slurp "src/day03/input"))
(def example (slurp "src/day03/example"))

(defn region [s idx hit]
  (when s
    (subs s
          (max 0 (dec idx))
          (min (+ idx (inc (count hit)))
               (count s)))))

(defn adjacent [s]
  (str/replace s #"\d|\." ""))

(defn check [[before line after]]
  (let [matcher (re-matcher #"\d+" line)]
    (loop [hits [] hit (re-find matcher)]
      (if hit
        (recur (conj hits
                     (let [idx (. matcher start)
                           r (str (region before idx hit)
                                  (region line idx hit)
                                  (region after idx hit))
                           a (adjacent r)]
                       [hit (= (count a) 1) r a before after line]))
               (re-find matcher))
        hits))))

(defn parse [input]
  (let [lines (str/split-lines input)
        groups (partition 3 1 lines)]
    (concat (check [nil (nth lines 0) (nth lines 1)])
            (mapcat check groups)
            (check [(nth lines (- (count lines) 2))
                    (nth lines (- (count lines) 1))
                    nil]))))

(defn part1 [parsed]
  (apply + (map (comp parse-long first) (filter (fn [[_ c]] c) parsed))))

(assert (= 4361 (part1 (parse example))))
(assert (= 529618 (part1 (parse input))))

;; (frequencies (map last (parse input)))
;; (apply + (map (comp parse-long first) (parse input)))
