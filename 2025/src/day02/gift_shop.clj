(ns day02.gift-shop
  (:require
   [clojure.math :as math]))

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

(defn divisors [v]
  (for [d (range 1 (inc (math/ceil (/ v 2))))
        :when (zero? (rem v d))]
    d))

(comment (divisors 100))

(defn invalid2? [id]
  (let [sid (str id)
        divs (divisors (count sid))]
    (when (> id 9)
      (some (fn [div]
              (when (= (count (set (re-seq (re-pattern (str ".{" div "}")) sid))) 1)
                id))
            divs))))

(defn search2 [[low high]]
  (keep invalid2? (range low (inc high))))

(defn part2 [in]
  (mapcat search2 in))

(assert (= 4174379265 (reduce + (part2 (parse example)))))
(assert (= 31898925685 (reduce + (part2 (parse input)))))
