(ns day22.monkey-market
  (:require [clojure.string :as str]))

(def input (slurp "src/day22/input"))
(def example (slurp "src/day22/example"))

(defn parse [in]
  (map parse-long (str/split-lines in)))

(defn mix [secret n]
  (bit-xor secret n))

(defn prune [secret]
  (mod secret 16777216))

(defn s1 [secret]
  (prune (mix secret (* 64 secret))))

(defn s2 [secret]
  (prune (mix secret (int (/ secret 32)))))

(defn s3 [secret]
  (prune (mix secret (* secret 2048))))

(defn step [secret]
  (s3 (s2 (s1 secret))))

(assert (= [123
            15887950
            16495136
            527345
            704524
            1553684
            12683156
            11100544
            12249484
            7753432
            5908254]
           (take 11 (iterate step 123))))

(defn part1 [secrets]
  (apply + (for [secret secrets]
             (nth (iterate step secret) 2000))))

(assert (= 37327623 (part1 (parse example))))
(assert (= 17577894908 (part1 (parse input))))

(defn digit1 [n]
  (- n (* 10 (int (/ n 10)))))

(assert (= [3 0 6 5 4 4 6 4 4 2]
           (map digit1 (take 10 (iterate step 123)))))

(defn changes [prices]
  (mapv (fn [[a b]] (- b a)) (partition 2 1 prices)))

(assert (= [-3 6 -1 -1 0 2 -2 0 -2]
           (changes (map digit1 (take 10 (iterate step 123))))))

(defn price-set [secret]
  (let [secrets (take 2010 (iterate step secret))
        prices (map digit1 secrets)
        deltas (changes prices)]
    (drop 4 (take 2001 (map-indexed (fn [i n]
                                      [[i (digit1 n)]
                                       (when (>= i 4)
                                         (reverse (for [d (range 1 5)]
                                                    (nth deltas (- i d)))))])
                                    secrets)))))

(price-set 123)
(price-set 1)

(defn price-index [secret]
  (let [ps (price-set secret)]
    (reduce (fn [db [[_idx value] delta]]
              (if (get db delta)
                db
                (assoc db delta value))) {} ps)))

(count (price-index 123))
(count (price-index 1))
(count (price-index 2))
(count (price-index 3))
(count (price-index 2024))

(merge-with (price-index 1) (price-index 2))

(defn part2 [secrets]
  (apply max (vals (reduce (fn [acc idx] (merge-with + acc idx)) {} (map price-index secrets)))))

(assert (= 23 (part2 [1 2 3 2024])))
(assert (= 1931 (part2 (parse input))))
