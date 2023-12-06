(ns day06.wait-for-it
  (:require [clojure.string :as str]))

(def input (slurp "src/day06/input"))
(def example (slurp "src/day06/example"))

(defn parse [in]
  (let [xs (map parse-long (re-seq #"\d+" in))
        [times dist] (split-at (/ (count xs) 2) xs)]
    (map vector times dist)))

(defn trial [duration hold]
  (* hold (- duration hold)))

(defn cases [[time dist]]
  (for [hold (range time)
        :let [d (trial time hold)]
        :when (> d dist)]
    [hold d]))

(defn part1 [in]
  (apply * (mapv (comp count cases) in)))

(assert (= 288 (part1 (parse example))))
(assert (= 4811940 (part1 (parse input))))

(defn parse2 [in]
  (let [xs (re-seq #"\d+" in)
        pieces (split-at (/ (count xs) 2) xs)]
    (map (fn [piece] (parse-long (apply str piece))) pieces)))

(defn search [check guess lower upper]
  (let [mid (quot (+ upper lower) 2)]
    (when (< lower guess upper)
      (println guess)
      (case (check guess)
        :hit guess
        :above (recur check (quot (+ mid lower) 2) lower mid)
        :below (recur check (quot (+ upper mid) 2) mid upper)))))

(defn part2 [[duration record]]
  (let [low (search (fn [h]
                      (let [r (trial duration h)]
                        (cond
                          (and (> r record) (<= (trial duration (dec h)) record)) :hit
                          (<= r record) :below
                          (> r record) :above)))
                    (quot duration 2) 0 duration)
        high (search (fn [h]
                       (let [r (trial duration h)]
                         (cond
                           (and (> r record) (<= (trial duration (inc h)) record)) :hit
                           (<= r record) :above
                           (> r record) :below)))
                     (quot duration 2) 0 duration)]
    [low high (inc (- high low))]))

(assert (= 71503 (last (part2 (parse2 example)))))
(assert (= 30077773 (last (part2 (parse2 input)))))

(search (fn [guess] (cond (< guess 4) :below
                         (> guess 4) :above
                         :else :hit))
        11 2 13)
