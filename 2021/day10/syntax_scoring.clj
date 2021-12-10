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

(defn check [syms]
  (let [s (walk syms)]
    (when (string? s)
      s)))

(defn part1 [input]
  (let [points {")" 3 "]" 57 "}" 1197 ">" 25137}]
    (apply + (map points (keep check input)))))

(assert (= 26397 (part1 (parse "example"))))
(assert (= 268845 (part1 (parse "input"))))




