(ns snailfish
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.zip :as z]))

(defn parse [file]
  (mapv edn/read-string (str/split-lines (slurp file))))

(defn left-of [z]
  (loop [loc z]
    (when loc
      (cond (and (not= z loc) (int? (z/node loc)))
            loc
            (z/left loc)
            (recur (z/left loc))
            :else
            (recur (z/up loc))))))

(defn right-to-zero [z]
  (loop [loc z]
    (if (= 0 (z/node loc))
      loc
      (recur (z/next loc)))))

(defn right-of [z]
  (loop [loc z]
    (when loc
      (cond (and (not= z loc) (int? (z/node loc)))
            loc
            (z/end? loc)
            nil
            :else
            (recur (z/next loc))))))

(defn explode-pair [snail]
  (loop [loc (z/vector-zip snail) depth 0]
    (cond (z/end? loc)
          nil
          (vector? (z/node loc))
          (if (and (> depth 3) (every? int? (z/node loc)))
            (let [[a b] (z/node loc)
                  explode (z/replace loc 0)
                  explode' (if-let [left (left-of explode)]
                             (-> (z/replace left (+ (z/node left) a))
                                 right-to-zero)
                             explode)]
              (z/root (if-let [right (right-of explode')]
                        (z/replace right (+ (z/node right) b))
                        explode')))
            (recur (z/down loc) (inc depth)))
          (int? (z/node loc))
          (if (z/right loc)
            (recur (z/right loc) depth)
            (recur (z/next loc) (dec depth))))))

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
    (if-let [step ((some-fn explode-pair split-pair) result)]
      (recur step)
      result)))

(assert (= [[1 2] [[3 4] 5]] (addition [1 2] [[3 4] 5])))
(assert (= [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
           (addition [[[[4,3],4],4],[7,[[8,4],9]]] [1 1])))

(defn homework [input]
  (reduce addition input))

(assert (= [[[[1,1],[2,2]],[3,3]],[4,4]] (homework [[1,1] [2,2] [3,3] [4,4]])))
(assert (= [[[[3,0],[5,3]],[4,4]],[5,5]] (homework [[1,1] [2,2] [3,3] [4,4] [5,5]])))
(assert (= [[[[5,0],[7,4]],[5,5]],[6,6]] (homework [[1,1] [2,2] [3,3] [4,4] [5,5] [6,6]])))

;; not working yet
(homework (parse "example2"))
(homework [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
           [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
           [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
           [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
           [7,[5,[[3,8],[1,4]]]]
           [[2,[2,2]],[8,[8,1]]]
           [2,9]
           [1,[[[9,3],9],[[9,0],[0,7]]]]
           [[[5,[7,4]],7],1]
           [[[[4,2],2],6],[8,7]]])
;; (defn magnitude [])
