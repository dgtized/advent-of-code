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
    (if (number? value)
      value
      (let [{:keys [op deps]} value]
        (apply op (map (partial evaluate graph) deps))))))

(defn star1 [file]
  (let [graph (parse file)]
    (evaluate graph "root")))

(defn simplify [graph]
  (let [path (reverse (aoc/a*-search (fn [x] (:deps (get graph x)))
                                     "root" "humn"))
        graph' (assoc-in graph ["root" :op] =)]
    (reduce (fn [pathing [child parent]]
              (let [{:keys [op deps]} (get graph' parent)
                    [lhs rhs] deps
                    other (if (= child lhs) rhs lhs)
                    value (evaluate graph' other)]
                (conj pathing
                      [parent {:op op
                               :deps (replace {other value} [lhs rhs])}])))
            [["humn" 0]]
            (map vector path (rest path)))))

(def inversion {+ - - + quot * * quot = =})
(def op->str {+ "+" - "-" quot "/" * "*"})

(defn reverse-eval [pathing]
  (let [pathing (reverse pathing)]
    (reductions (fn [[value _] [node {:keys [op deps]}]]
                  (if (= node "root")
                    [(first (filter number? deps)) "root"]
                    (let [operator (get inversion op)
                          [lhs _] deps
                          deps' (cond (and (number? lhs) (contains? #{quot -} operator))
                                      [value lhs]
                                      (and (number? lhs) (= op -))
                                      [lhs (- value)]
                                      :else
                                      (map (fn [v] (if (string? v) value v))
                                           deps))]
                      [(apply operator deps')
                       node
                       (op->str operator)
                       deps' (op->str op) deps
                       (apply operator (map (fn [v] (if (string? v) value v)) deps))])))
                [0 0]
                (butlast pathing))))

#_(reverse-eval (simplify example))
#_(reverse-eval (simplify input))

(defn star2 [file]
  (let [graph (parse file)
        root (get graph "root")
        [rhs lhs] (:deps root)
        expected (evaluate graph lhs)
        human (first (last (reverse-eval (simplify graph))))
        result (evaluate (assoc graph "humn" human) rhs)]
    {:human human
     :result result
     :expected expected
     :checksum (= result expected)}))

#_(star2 "input/day21.example")
#_(star2 "input/day21.input")

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day21")
 (fn [{:keys [result]}] result))
