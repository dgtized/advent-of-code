(ns day12.hot-springs
  (:require [clojure.string :as str]))

(def input (slurp "src/day12/input"))
(def example-good (slurp "src/day12/example.good"))
(def example (slurp "src/day12/example.bad"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[springs numbers] (str/split line #"\s+")]
      [springs (map parse-long (str/split numbers #","))])))

(defn accept? [s nums]
  (let [groups (re-seq #"#+" s)]
    (= (mapv count groups) nums)))

(comment
  (accept? "#.##.##" [1 2 2]))

(defn accept-partial? [s nums]
  (let [groups (re-seq #"#+" s)]
    (= (mapv count (butlast groups)) (take (max 0 (dec (count groups))) nums))))

(comment
  (accept-partial? "#." [2 1]))

(defn generate-n [[s nums]]
  (get (frequencies (seq s)) \? 0))

(defn generate [springs nums]
  (loop [coll [""] springs springs]
    (if-let [s (first springs)]
      (recur (keep
              (fn [gen] (when (accept-partial? gen nums)
                         gen))
              (if (= s \?)
                (mapcat (fn [before] [(str before ".") (str before "#")]) coll)
                (map (fn [before] (str before s)) coll)))
             (rest springs))
      coll)))

(comment
  (filter (fn [s] (accept? s [3 2 1])) (generate "?###????????" [3,2,1])))

(defn part1 [in]
  (apply +
         (for [[springs nums] in]
           (reduce (fn [s e] (if (accept? e nums)
                              (inc s)
                              s))
                   0 (generate springs nums)))))

(assert (= 21 (part1 (parse example))))
(time (assert (= 7191 (part1 (parse input)))))

;; (defn part2 [in]
;;   in)

;; (assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
