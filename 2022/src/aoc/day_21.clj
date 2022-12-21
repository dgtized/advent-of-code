(ns aoc.day-21
  (:require
   [aoc.utility :as aoc]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 21 :name "example" :content "")
#_(aoc/day-input :day 21)

(defn parse-eq [s]
  (cond (re-matches #"\d+" s)
        (parse-long s)
        (re-matches #"(\w+) ([\-*+/]) (\w+)" s)
        (let [[lhs op rhs] (rest (re-matches #"(\w+) ([\-*+/]) (\w+)" s))]
          {:op (get {"+" + "-" - "/" quot "*" *} op)
           :deps [lhs rhs]})
        :else [[s]]))

(defn parse [file]
  (into {}
        (for [line (aoc/file->lines file)]
          (let [[monkey eq] (str/split line #": ")]
            [monkey (parse-eq eq)]))))

(def example (parse "input/day21.example"))
(def input (parse "input/day21.input"))

(defn evaluate [graph monkey]
  (let [value (get graph monkey)]
    (println "e: " monkey value)
    (if (number? value)
      value
      (let [{:keys [op deps]} value]
        (apply op (map (partial evaluate graph) deps))))))

(defn star1 [file]
  (let [graph (parse file)]
    (evaluate graph "root")))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day21")
 (fn [{:keys [result]}] result))
