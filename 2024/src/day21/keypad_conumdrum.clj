(ns day21.keypad-conumdrum
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.graph :as graph]
            [aoc.vector :as v]))

(def input (slurp "src/day21/input"))
(def example (slurp "src/day21/example"))

(def keypad
  (-> "
789
456
123
 0A"
      str/split-lines
      ag/lines->grid
      (dissoc [0 4])))

(def dirpad
  (-> "
 ^A
<v>
"
      str/split-lines
      ag/lines->grid
      (dissoc [0 0])))

(defn translate [dir]
  (case dir
    [0 -1] \^
    [0 1] \v
    [-1 0] \<
    [1 0] \>))

(defn find-next [grid start value]
  (let [path (graph/a*-search
              {:successors (fn [pos]
                             (for [dir v/cardinal
                                   :let [loc (v/v+ pos dir)]
                                   :when (get grid loc)]
                               loc))
               :sources [start]
               :goal? (fn [pos] (= (get grid pos) value))})]
    {:key-pos (last path)
     :path path
     :directions (apply str (map translate (mapv (fn [[p q]] (v/v- q p)) (partition 2 1 path))))}))

(defn find-start [grid]
  (ag/some-value grid \A))

(comment (find-next keypad (find-start keypad) \0))

(defn path [grid code]
  (loop [code (seq code) pos (find-start grid) path []]
    (if (seq code)
      (let [{:keys [key-pos directions]} (find-next grid pos (first code))]
        (recur (rest code) key-pos (conj path directions "A")))
      (str/join "" path))))

;;          <A^A>^^AvvvA
(assert (= "<A^A>^^AvvvA"
           (path keypad "029A")))
;;          v<<A>>^A<A>AvA<^AA>A<vAAA>^A
(assert (= "v<<A>>^A<A>AvA^<AA>Av<AAA>^A" (path dirpad (path keypad "029A"))))
;;          <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
(assert (= "v<A<AA>>^AvAA^<A>Av<<A>>^AvA^Av<A>^A<Av<A>>^AAvA^Av<A<A>>^AAAvA^<A>A"
           (path dirpad (path dirpad (path keypad "029A")))))

(defn translate-path [code]
  (path dirpad (path dirpad (path keypad code))))

;;          <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
(assert (= "v<<A>>^Av<A<A>>^AAvA^<A>AvA^Av<A>^A<A>Av<A>^A<A>Av<A<A>>^AAvA^<A>A"
           (translate-path "456A")))

;;          <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
(assert (= "v<<A>>^AvA^Av<<A>>^AAv<A<A>>^AAvAA^<A>Av<A>^AA<A>Av<A<A>>^AAAvA^<A>A"
           (translate-path "379A")))

(defn parse [in]
  (str/split-lines in))

(defn part1 [codes]
  (let [paths
        (for [code codes]
          (let [code-path (translate-path code)]
            {:code code :c (parse-long (re-find #"^\d+" code))
             :path code-path :n (count code-path)}))]
    {:paths paths
     :score (apply + (for [{:keys [c n]} paths] (* c n)))}))

;; (assert (= 126384 (part1 (parse example))))
;; (assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
