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

(defn find-start [grid]
  (ag/some-value grid \A))

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

(defn find-paths [grid start value]
  (let [goal (ag/some-value grid value)]
    {:key-pos goal
     :directions
     (into []
           (for [p (bfs-paths {:successors (successors grid)
                               :source start
                               :goal goal
                               :visited {}
                               :path []})]
             (apply str (map translate (mapv (fn [[p q]] (v/v- q p)) (partition 2 1 p))))))}))

(comment (find-paths keypad (find-start keypad) \0)
         (find-paths keypad (find-start keypad) \1))

(defn find-next [grid start value]
  (let [path (graph/a*-search
              {:successors (successors grid)
               :sources [start]
               :goal? (fn [pos] (= (get grid pos) value))})]
    {:key-pos (last path)
     :path path
     :directions (apply str (map translate (mapv (fn [[p q]] (v/v- q p)) (partition 2 1 path))))}))

(comment (find-next keypad (find-start keypad) \0))

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
  (loop [code (seq code) pos (find-start grid) path []]
    (if (seq code)
      (let [{:keys [key-pos directions]} (find-paths grid pos (first code))]
        (recur (rest code) key-pos (conj path directions ["A"])))
      (expand-paths (collapse-subpaths path)))))

(defn dir-paths [grid codes]
  (let [all-paths (sort-by count (mapcat (fn [code] (paths grid code)) codes))
        len (+ 4 (count (first all-paths)))]
    (filter (fn [p] (<= (count p) len)) all-paths)))

(defn best-path [paths]
  (count (first paths)))

;;          <A^A>^^AvvvA
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
;; "Elapsed time: 18063.124295 msecs"
;; "Elapsed time: 19644.934528 msecs"
;; "Elapsed time: 4028.190723 msecs"
;; "Elapsed time: 16630.604651 msecs"
;; "Elapsed time: 12901.001107 msecs"
;; (assert (= 184718 (:score (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
