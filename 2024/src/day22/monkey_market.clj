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

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
