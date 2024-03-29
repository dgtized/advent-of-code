(ns snailfish
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as z]))

(defn parse [file]
  (mapv edn/read-string (str/split-lines (slurp file))))

(defn left-of [z]
  (loop [loc z]
    (when loc
      (if (and (not= z loc) (int? (z/node loc)))
        loc
        (recur (z/prev loc))))))

(defn return-to-explode
  [loc]
  (->> (iterate z/next loc)
       (drop-while #(not= 0 (z/node %)))
       first))

(defn right-of [z]
  (loop [loc z]
    (when loc
      (cond (and (not= z loc) (int? (z/node loc)))
            loc
            (z/end? loc)
            nil
            :else
            (recur (z/next loc))))))

(defn depth [loc]
  (dec (count (take-while some? (iterate z/up loc)))))

(defn explode-pair [snail]
  (loop [loc (z/vector-zip snail)]
    (cond (z/end? loc)
          nil
          (and (= (depth loc) 4) (vector? (z/node loc)))
          (let [[a b] (z/node loc)
                explode (z/replace loc 0)
                explode' (if-let [left (left-of explode)]
                           (-> left
                               (z/edit + a)
                               return-to-explode)
                           explode)]
            (z/root (if-let [right (right-of explode')]
                      (z/edit right + b)
                      explode')))
          :else
          (recur (z/next loc)))))

(assert (not (explode-pair [[[[0,9],2],3],4])))
(assert (= [[[[0,9],2],3],4] (explode-pair [[[[[9,8],1],2],3],4])))
(assert (= [7,[6,[5,[7,0]]]] (explode-pair [7,[6,[5,[4,[3,2]]]]])))
(assert (= [[6,[5,[7,0]]],3] (explode-pair [[6,[5,[4,[3,2]]]],1])))
(assert (= [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
           (explode-pair [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])))
(assert (= [[3,[2,[8,0]]],[9,[5,[7,0]]]]
           (explode-pair [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])))

(defn split-pair [snail]
  (loop [loc (z/vector-zip snail)]
    (let [value (z/node loc)]
      (cond (z/end? loc)
            nil
            (and (int? value) (>= value 10))
            (-> loc
                (z/replace [(int (Math/floor (/ value 2.0)))
                            (int (Math/ceil (/ value 2.0)))])
                z/root)
            :else
            (recur (z/next loc))))))

(assert (not (split-pair [9])))
(assert (= [[5 5]] (split-pair [10])))
(assert (= [[5 6]] (split-pair [11])))
(assert (= [[6 6]] (split-pair [12])))

(defn addition [lhs rhs]
  (loop [result (vector lhs rhs)]
    (if-let [exploded (explode-pair result)]
      (recur exploded)
      (if-let [split (split-pair result)]
        (recur split)
        result))))

(assert (= [[1 2] [[3 4] 5]] (addition [1 2] [[3 4] 5])))
(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
           (addition [[[[4,3],4],4],[7,[[8,4],9]]] [1 1])))

(defn homework [input]
  (reduce addition input))

(assert (= [[[[1,1],[2,2]],[3,3]],[4,4]] (homework [[1,1] [2,2] [3,3] [4,4]])))
(assert (= [[[[3,0],[5,3]],[4,4]],[5,5]] (homework [[1,1] [2,2] [3,3] [4,4] [5,5]])))
(assert (= [[[[5,0],[7,4]],[5,5]],[6,6]] (homework [[1,1] [2,2] [3,3] [4,4] [5,5] [6,6]])))

(assert (= [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
           (addition [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                     [7,[[[3,7],[4,3]],[[6,3],[8,8]]]])))
(assert (= [[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]
           (addition [[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]
                     [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]])))
(assert (= [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]
           (homework (parse "long-example"))))

(defn magnitude [snail]
  (cond (int? snail)
        snail
        (vector? snail)
        (+ (* 3 (magnitude (first snail)))
           (* 2 (magnitude (second snail))))))

(assert (= 29 (magnitude [9 1])))
(assert (= 21 (magnitude [1 9])))
(assert (= 143 (magnitude [[1,2],[[3,4],5]])))
(assert (= 3488 (magnitude [[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])))

(assert (= 4140 (magnitude (homework (parse "example2")))))
(assert (= 3051 (magnitude (homework (parse "input")))))

(defn part2 [input]
  (apply max (for [a input
                   b input
                   :when (not= a b)]
               (magnitude (addition a b)))))

(assert (= 3993 (part2 (parse "example2"))))
(assert (= 4812 (part2 (parse "input"))))
