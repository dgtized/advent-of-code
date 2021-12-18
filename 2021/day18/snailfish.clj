(ns snailfish
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as z]))

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

(comment
  (-> (z/vector-zip [1 2 [3 4] 5])
      z/next
      z/up
      z/up)

  (-> (z/vector-zip [1 [2 [3 [4 [5 0]]]]])
      z/next
      z/next
      left-of
      (z/replace 0)
      z/root))

(defn left-of [z]
  (loop [loc z]
    (cond (and (not= z loc) (int? (z/node loc)))
          loc
          (z/left loc)
          (recur (z/left loc))
          :else
          (recur (z/up loc)))))

(defn right-to-zero [z]
  (loop [loc z]
    (if (= 0 (z/node loc))
      loc
      (recur (z/next loc)))))

(defn right-of [z]
  (loop [loc z]
    (cond (and (not= z loc) (int? (z/node loc)))
          loc
          (z/end? loc)
          nil
          :else
          (recur (z/next loc)))))

(defn explode-pair [snail]
  (loop [loc (z/vector-zip snail) depth 0]
    (cond (z/end? loc)
          (z/node loc)
          (and (> depth 3) (vector? (z/node loc)))
          (let [[a b] (z/node loc)
                explode (z/replace loc 0)
                explode' (if-let [left (left-of explode)]
                           (do (println left)
                               (-> (z/replace left (+ (z/node left) a))
                                   right-to-zero))
                           explode)]
            (z/root (if-let [right (right-of explode')]
                      (z/replace right (+ (z/node right) b))
                      explode')))

          (vector? (z/node loc))
          (recur (z/down loc) (inc depth))
          (int? (z/node loc))
          (recur (z/next loc) depth))))

(assert (= [[[[0,9],2],3],4] (explode-pair [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]] (explode-pair [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3] (explode-pair [[6,[5,[4,[3,2]]]],1])))

(defn split-pair [snail]
  snail)
