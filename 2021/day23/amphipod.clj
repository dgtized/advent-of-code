(ns amphipod
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [lines (str/split-lines (slurp filename))]
    (into {} (for [[j line] (map-indexed vector lines)
                   [i c] (map-indexed vector (seq line))
                   :when (not= c \ )]
               [[i j] c]))))

(defn v+ [a b]
  (mapv + a b))

(defn neighbors [board pos]
  (for [change [[-1 0] [0 -1] [0 1] [1 0]]
        :let [neighbor (v+ pos change)
              value (get board neighbor)]
        :when value]
    [neighbor value]))

(assert (= [[[6 3] \#] [[7 2] \B] [[7 4] \#] [[8 3] \#]]
           (neighbors (parse "example") [7 3])))

(defn pieces [board]
  (filter (fn [[_ v]] (#{\A \B \C \D} v)) board))

(defn movable-pieces [board]
  (filter (fn [[p _]]
            (some (fn [[_ val]] (= \. val)) (neighbors board p)))
          (pieces board)))

(assert (= [[[7 2] \B] [[5 2] \C] [[9 2] \D] [[3 2] \B]]
           (movable-pieces (parse "example"))))

(defn open-spaces [board]
  (mapv first (filter (fn [[_ v]] (= \. v)) board)))

(assert (= 11 (count (open-spaces (parse "example")))))

(let [expected (update-vals (group-by second (pieces (parse "result")))
                            #(set (map first %)))]
  (defn correct-pieces [board]
    (reduce (fn [s [p v]] (if ((get expected v) p)
                           (conj s [p v])
                           s))
            #{}
            (sort-by second (pieces board)))))

(assert (= #{[[3 3] \A] [[9 2] \D] [[7 3] \C]}
           (correct-pieces (parse "example"))))

;; (defn legal-moves [board [i j]])
;; (defn valuation [board])
;; (defn )
