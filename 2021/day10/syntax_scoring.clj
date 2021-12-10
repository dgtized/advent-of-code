(ns syntax-scoring
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv str (seq line)))

(defn parse [filename]
  (mapv parse-line (str/split-lines (slurp filename))))

(def pairs {"[" "]"
            "(" ")"
            "{" "}"
            "<" ">"})

(def open (set (keys pairs)))
(def close (set (vals pairs)))

(defn walk [syms]
  (reduce (fn [stack sym]
            (cond (open sym)
                  (conj stack sym)
                  (close sym)
                  (if (= sym (get pairs (peek stack)))
                    (pop stack)
                    (reduced sym))))
          [] syms))

(defn part1 [input]
  (let [points {")" 3 "]" 57 "}" 1197 ">" 25137}]
    (->> input
         (mapv walk)
         (filter string?)
         (map points)
         (apply +))))

(assert (= 26397 (part1 (parse "example"))))
(assert (= 268845 (part1 (parse "input"))))

(defn part2 [input]
  (let [points {")" 1 "]" 2 "}" 3 ">" 4}]
    (->> input
         (mapv walk)
         (remove string?)
         (mapv (comp (partial map pairs) reverse))
         (mapv (partial reduce (fn [acc sym] (+ (* acc 5) (get points sym))) 0)))))

(assert (= [288957 5566 1480781 995444 294] (part2 (parse "example"))))

(defn middle [xs]
  (let [n (count xs)]
    (nth (sort xs) (/ n 2))))

(assert (= 288957 (middle (part2 (parse "example")))))
(assert (= 4038824534 (middle (part2 (parse "input")))))




