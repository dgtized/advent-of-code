(ns day02.gift-shop)

(def input (slurp "src/day02/input"))
(def example (slurp "src/day02/example"))

(defn parse [in]
  (for [m (re-seq #"(\d+)-(\d+)" in)]
    (vec (mapv parse-long (rest m)))))

(defn invalid? [id]
  (let [sid (str id)
        mid (/ (count sid) 2.0)]
    (if (even? (count sid))
      (if (or (= (subs sid 0 mid) (subs sid mid)))
        id
        nil)
      nil)
    ))

(defn search [[low high]]
  (keep invalid? (range low (inc high))))

(defn part1 [in]
  (mapcat search in))

(assert (= 1227775554 (reduce + (part1 (parse example)))))
(assert (= 30608905813 (reduce + (part1 (parse input)))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
