(ns snailfish
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(defn parse [file]
  (mapv edn/read-string (str/split-lines (slurp file))))

(parse "example")

(defn addition [lhs rhs]
  (vector lhs rhs))

(addition [1 2] [[3 4] 5])

(defn tree-depth [snail]
  (letfn [(walk [depth node]
            (lazy-seq (cons [node depth]
                            (when (vector? node)
                              (mapcat (partial walk (inc depth)) node)))))]
    (filter (comp int? first) (walk 0 snail))))

(tree-depth [[[[[9,8],1],2],3],4])
(tree-depth [7,[6,[5,[4,[3,2]]]]])
(tree-depth [[6,[5,[4,[3,2]]]],1])

(zip/down (zip/vector-zip [[[[[9,8],1],2],3],4]))

(defn explode-pair [snail]
  (tree-seq seq? identity snail))

(assert (= [[[[0,9],2],3],4] (explode-pair [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]] (explode-pair [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3] (explode-pair [[6,[5,[4,[3,2]]]],1])))

(defn split-pair [snail]
  snail)
