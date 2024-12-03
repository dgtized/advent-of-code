(ns day03.day03)

(def input (slurp "src/day03/input"))
(def example (slurp "src/day03/example"))

(defn parse [in]
  (for [m (re-seq #"mul\((\d+),(\d+)\)" in)]
    (let [[a b] (map parse-long (rest m))]
      [a b])))

(defn part1 [in]
  (apply + (map (fn [[a b]] (* a b)) in)))

(assert (= 161 (part1 (parse example))))
(assert (= 166630675 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
