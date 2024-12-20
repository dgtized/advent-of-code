(ns day12.garden-groups
  (:require [clojure.string :as str]
            [aoc.grid :as ag]
            [aoc.vector :as v]))

(def input (slurp "src/day12/input"))
(def example (slurp "src/day12/example"))
(def example2 (slurp "src/day12/example2"))
(def example3 (slurp "src/day12/example3"))
(def example_e (slurp "src/day12/example_e"))

(defn parse [in]
  (->> in
       str/split-lines
       ag/lines->grid))

(defn neighbors [grid cell]
  (for [dir v/cardinal
        :let [loc (v/v+ cell dir)]
        :when (get grid loc)]
    loc))

(defn successors [cell]
  (for [dir v/cardinal] (v/v+ cell dir)))

(defn flood [grid start]
  (let [contents (get grid start)]
    (loop [region #{start} frontier (neighbors grid start)]
      (if (empty? frontier)
        region
        (let [accepted (filter #(= (get grid %) contents) frontier)
              region' (into region accepted)]
          (recur region'
                 (distinct (mapcat (fn [cell] (remove region' (neighbors grid cell))) accepted))))))))

(comment (flood (parse example2) [0 0]))

(defn segment [grid]
  (loop [regions [] grid grid]
    (if (empty? grid)
      regions
      (let [region (flood grid (ffirst grid))]
        (recur (conj regions region)
               (apply dissoc grid region))))))

(comment (segment (parse example))
         (segment (parse example2))
         (segment (parse example3)))

(defn perimeter [region]
  (reduce (fn [perimeter cell]
            (+ perimeter (count (remove region (successors cell)))))
          0
          region))

(comment (map perimeter (segment (parse example))))

(defn price [region]
  (* (count region) (perimeter region)))

(defn part1 [in]
  (apply + (map price (segment in))))

(assert (= 1930 (part1 (parse example))))
(assert (= 1431316 (part1 (parse input))))

;; (defn sides [grid region]
;;   (let [side-cells (remove (fn [cell] (every? region (successors cell))) region)]
;;     (segment (select-keys grid side-cells))))

(defn contiguous
  "Find contiguous positions where elements increment by 1 in `field`.

  Ie [[1 1] [2 1] [3 1]] is contigous because x is incrementing."
  [field coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (->> s
                    first
                    field
                    (iterate inc)
                    (map vector s)
                    (take-while (fn [[el v]] (= (field el) v)))
                    (mapv first))]
       (cons run (contiguous field (lazy-seq (drop (count run) s))))))))

(comment (contiguous first [[0 1] [1 1] [2 1] [4 1] [5 1] [8 1]]))

(defn sides [region]
  (apply +
         (for [[dir cont-f axis-f]
               [[[0 -1] first second]
                [[1 0] second first]
                [[0 1] first second]
                [[-1 0] second first]]]
           (->> region
                ;; focus on cells that have a neighbor in a cardinal direction
                ;; that are outside the region.
                (remove (fn [cell] (contains? region (v/v+ cell dir))))
                ;; group along axis and then look for contigous segments
                (sort-by (juxt axis-f cont-f))
                ;; group segments with the same axis field
                (partition-by axis-f)
                ;; chunk up contigous segments on a given axis
                (mapcat (fn [group] (contiguous cont-f group)))
                count))))

(let [grid (parse example_e)]
  (map (fn [region] (sides region)) (segment grid)))

(defn part2 [in]
  (apply + (map (fn [region] (* (count region) (sides region)))
                (segment in))))

(assert (= 1206 (part2 (parse example))))
(assert (= 821428 (part2 (parse input))))
