(ns day05.print-queue
  (:require [clojure.string :as str]))

(def input (slurp "src/day05/input"))
(def example (slurp "src/day05/example"))

(defn parse [in]
  (let [[rules page-updates] (str/split in #"\n\n")]
    {:rules
     (->> rules str/split-lines
          (mapv (fn [line] (mapv parse-long (str/split line #"\|"))))
          (reduce (fn [rule-set [a b]] (update rule-set a (fnil conj #{}) b)) {}))
     :pages
     (->> page-updates str/split-lines
          (mapv (fn [line] (mapv parse-long (re-seq #"\d+" line)))))}))

(defn legal? [rules xs]
  (every? (fn [[p1 p2]] (not (contains? (get rules p2 #{}) p1))) (partition 2 1 xs)))

(defn middle [xs]
  (nth xs (/ (count xs) 2)))

(defn part1 [{:keys [rules pages]}]
  (apply + (map middle (filter #(legal? rules %) pages))))

(assert (= 143 (part1 (parse example))))
(assert (= 5509 (part1 (parse input))))

(defn legalize [rules xs]
  (sort (fn [a b]
          (let [sa (get rules a #{})
                sb (get rules b #{})]
            (cond (contains? sa b)
                  -1
                  (contains? sb a)
                  1
                  :else 0)))
        xs))

(defn part2 [{:keys [rules pages]}]
  (->> pages
       (remove #(legal? rules %))
       (map #(legalize rules %))
       (map middle)
       (apply +)))

(assert (= 123 (part2 (parse example))))
(assert (= 4407 (part2 (parse input))))
