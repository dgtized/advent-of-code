(ns day21.keypad-conumdrum
  (:require
   [aoc.combinatorics :as ac]
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
      path)))

(defn dir-paths [chunks]
  (mapcat (fn [chunk]
            (let [chunk-set (map (fn [code] (collapse-subpaths (paths dirpad code))) chunk)]
              (let [r (mapcat expand-paths chunk-set)
                    n (count (apply min-key count r))]
                [(filter #(= (count %) n) r)])))
          chunks))

(contains? (set (expand-paths (dir-paths (paths keypad "029A"))))
           "v<<A>>^A<A>AvA<^AA>A<vAAA>^A")

(contains? (set (expand-paths (dir-paths (dir-paths (paths keypad "029A")))))
           "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")

(assert (= ["<A^A^^>AvvvA"
            "<A^A^>^AvvvA"
            "<A^A>^^AvvvA"]
           (expand-paths (collapse-subpaths (paths keypad "029A")))))
(assert (contains? (set (expand-paths (dir-paths (paths keypad "029A"))))
                   "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"))
(assert (contains? (set (expand-paths (dir-paths (dir-paths (paths keypad "029A")))))
                   "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"))

(defn translate-paths [code depth]
  (expand-paths (nth (iterate dir-paths (paths keypad code)) depth)))

(assert (contains? (set (translate-paths "980A" 2))
                   "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A"))
(assert (contains? (set (translate-paths "456A" 2))
                   "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A"))
(assert (contains? (set (translate-paths "179A" 2))
                   "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"))
(assert (contains? (set (translate-paths "379A" 2))
                   "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"))

(defn parse [in]
  (str/split-lines in))

(defn part1 [codes depth]
  (let [best-paths
        (for [code codes]
          (let [code-path (apply min-key count (time (translate-paths code depth)))]
            {:code code :c (parse-long (re-find #"^\d+" code))
             :path code-path :n (count code-path)}))]
    {:paths best-paths
     :score (apply + (for [{:keys [c n]} best-paths] (* c n)))}))

(assert (= 126384 (:score (part1 (parse example) 2))))
;; "Elapsed time: 537.688884 msecs"
;; "Elapsed time: 20.562469 msecs"
;; "Elapsed time: 18.306883 msecs"
;; "Elapsed time: 161.662897 msecs"
;; "Elapsed time: 66.388046 msecs"
(println)
(assert (= 184718 (:score (part1 (parse input) 2))))

;; (assert (= (part1 (parse example) 25)))
;; (assert (= (part1 (parse input) 25)))


(into {} (for [[a b] (ac/all-pairs (vals keypad))]
           [[a b] (find-paths keypad a b)]))

(into {} (for [[a b] (ac/all-pairs (vals dirpad))]
           [[a b] (map (fn [p]
                         (paths dirpad p)) (find-paths dirpad a b))]))

(defn tclose []
  [(into {} (for [[k v] dirpad]
              [v (vec (paths dirpad (str v)))]))
   (into {} (for [[k v] dirpad]
              (let [r (mapcat (fn [p] (paths dirpad p)) (paths dirpad (str v)))]
                [v
                 {:n (count r)
                  :f (frequencies (map count r))
                  :c r}])))
   (into {} (for [[k v] dirpad]
              (let [r (mapcat (fn [p] (paths dirpad p)) (mapcat (fn [p] (paths dirpad p)) (paths dirpad (str v))))]
                [v
                 {:n (count r)
                  :f (frequencies (map count r))
                  ;; :c r
                  }])))])
