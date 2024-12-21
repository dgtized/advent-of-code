(ns day21.keypad-conumdrum
  (:require
   [aoc.graph :as graph]
   [aoc.grid :as ag]
   [aoc.vector :as v]
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "src/day21/input"))
(def example (slurp "src/day21/example"))

(def keypad
  (-> "789
456
123
 0A"
      str/split-lines
      ag/lines->grid
      (dissoc [0 3])))

(def dirpad
  (-> " ^A
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

(defn translate-path [pts]
  (apply str (map translate (mapv (fn [[p q]] (v/v- q p)) (partition 2 1 pts)))))

(defn bfs-paths [{:keys [successors source goal]}]
  (loop [paths [[source]] visited #{source}]
    (if (contains? visited goal)
      (filter #(= (last %) goal) paths)
      (let [frontier (mapcat (fn [path]
                               (for [n (successors (last path))]
                                 (when-not (contains? visited n)
                                   (conj path n))))
                             paths)]
        (recur frontier (set/union visited (set (map last frontier))))))))

(defn successors [grid]
  (fn [pos] (for [dir v/cardinal
                 :let [loc (v/v+ pos dir)]
                 :when (get grid loc)]
             loc)))

(defn find-paths [grid src dst]
  (let [start (ag/some-value grid src)
        goal (ag/some-value grid dst)]
    (->> {:successors (successors grid)
          :source start
          :goal goal
          :visited {}
          :path []}
         bfs-paths
         (map translate-path)
         (into []))))

(comment (find-paths keypad \A \0)
         (find-paths keypad \A \1))

(defn collapse-subpaths [path]
  (loop [segments path accepted []]
    (if (seq segments)
      (if (= (count (first segments)) 1)
        (let [chunk (take-while (fn [segment] (= (count segment) 1)) segments)]
          (recur (drop (count chunk) segments)
                 (conj accepted (apply str (apply concat chunk)))))
        (recur (rest segments)
               (conj accepted (first segments))))
      accepted)))

(defn expand-paths [segments]
  (loop [segments segments paths [[]]]
    (if (seq segments)
      (let [choices (first segments)]
        (if (string? choices)
          (recur (rest segments)
                 (map (fn [path] (conj path choices)) paths))
          (recur (rest segments)
                 (mapcat (fn [path]
                           (for [choice choices] (into path choice)))
                         paths))))
      (mapv #(apply str %) paths))))

(defn paths [grid code]
  (loop [code (seq code) pos \A path []]
    (if (seq code)
      (let [next-pos (first code)
            directions (find-paths grid pos next-pos)]
        (recur (rest code) next-pos (conj path (map (fn [d] (str d "A")) directions))))
      (expand-paths (collapse-subpaths path)))))

(defn dir-paths [grid codes]
  (let [all-paths (mapcat (fn [code] (paths grid code)) codes)
        len (count (apply min-key count all-paths))]
    (filter (fn [p] (= (count p) len)) all-paths)))

(assert (= ["<A^A^^>AvvvA"
            "<A^A^>^AvvvA"
            "<A^A>^^AvvvA"]
           (paths keypad "029A")))
(assert (contains? (set (dir-paths dirpad (paths keypad "029A")))
                   "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"))
(assert (contains? (set (dir-paths dirpad (dir-paths dirpad (paths keypad "029A"))))
                   "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"))

(defn translate-paths [code]
  (dir-paths dirpad (dir-paths dirpad (paths keypad code))))

(assert (contains? (set (translate-paths "980A"))
                   "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"))
(assert (contains? (set (translate-paths "456A"))
                   "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"))
(assert (contains? (set (translate-paths "179A"))
                   "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"))
(assert (contains? (set (translate-paths "379A"))
                   "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"))

(defn parse [in]
  (str/split-lines in))

(defn part1 [codes]
  (let [best-paths
        (for [code codes]
          (let [code-path (apply min-key count (time (translate-paths code)))]
            {:code code :c (parse-long (re-find #"^\d+" code))
             :path code-path :n (count code-path)}))]
    {:paths best-paths
     :score (apply + (for [{:keys [c n]} best-paths] (* c n)))}))

(assert (= 126384 (:score (part1 (parse example)))))
;; "Elapsed time: 14325.547546 msecs"
;; "Elapsed time: 3510.867153 msecs"
;; "Elapsed time: 4144.721302 msecs"
;; "Elapsed time: 2846.716222 msecs"
;; "Elapsed time: 467.9332 msecs"
;; (assert (= 184718 (:score (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
