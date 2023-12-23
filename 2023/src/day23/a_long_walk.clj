(ns day23.a-long-walk
  (:require
   [clojure.string :as str]))

(def input (slurp "src/day23/input"))
(def example (slurp "src/day23/example"))

(defn ->grid [input]
  (into {} (for [[j line] (map-indexed vector input)
                 [i c] (map-indexed vector line)]
             [[i j] c])))

(defn parse [in]
  (->grid (str/split-lines in)))

(defn dims [grid]
  [(inc (apply max (map first (keys grid))))
   (inc (apply max (map second (keys grid))))])

(defn exit [grid]
  (let [[mx my] (dims grid)]
    [(- mx 2) (- my 1)]))

(defn paths [successors path goal?]
  (let [current (peek path)]
    (if (goal? current)
      [path]
      (lazy-seq
       (->> (successors current)
            (remove (set path))
            (mapcat (fn [node] (paths successors (conj path node) goal?))))))))

(defn v+ [a b] (mapv + a b))

(defn successors [grid pos]
  (for [dir (case (get grid pos)
              \> [[1 0]]
              \< [[-1 0]]
              \^ [[0 -1]]
              \v [[0 1]]
              [[1 0] [-1 0] [0 -1] [0 1]])
        :let [pos' (v+ pos dir)]
        :when (let [v (get grid pos')]
                (and v (not= v \#)))]
    pos'))

(comment (successors (parse example) [1 0]))

(defn draw-grid [grid]
  (let [[mx my] (dims grid)]
    (doseq [j (range my)]
      (println (apply str
                      (for [i (range mx)]
                        (get grid [i j])))))))

(defn part1 [grid]
  (let [path
        (->> #(= (exit grid) %)
             (paths (partial successors grid) [[1 0]])
             (apply max-key count)
             rest)]
    (draw-grid (reduce (fn [g p] (assoc g p \O)) grid path))
    (count path)))

(assert (= 94 (part1 (parse example))))
;; (assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
