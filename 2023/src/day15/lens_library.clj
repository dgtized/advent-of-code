(ns day15.lens-library
  (:require [clojure.string :as str]))

(def input (slurp "src/day15/input"))
(def example (slurp "src/day15/example"))

(defn parse [in]
  (str/split (str/trim in) #","))

(defn checksum [in]
  (reduce (fn [curr v]
            (rem (* 17 (+ curr (byte v))) 256))
          0
          (seq in)))

(assert (= 52 (checksum "HASH")))

(defn part1 [in]
  (apply + (map checksum in)))

(assert (= 1320 (part1 (parse example))))
(assert (= 515210 (part1 (parse input))))

(defn parse-lens [lens]
  (when-let [m (re-find #"^(\w+)([-=])(\d*)" lens)]
    (let [[label op val] (rest m)]
      [label op (parse-long val)])))

(mapv parse-lens (parse input))

(defn update-box [box lens]
  (let [[label op val] lens]
    (vec (if (= op "-")
           (remove (fn [l] (= (first l) label)) box)
           (if-let [existing (some (fn [l] (when (= (first l) label) l)) box)]
             (replace {existing [label val]} box)
             (conj box [label val]))))))

(defn hash-boxes [in]
  (reduce (fn [boxes lens]
            (update boxes (checksum (first lens)) (fnil update-box []) lens))
          (vec (repeat 256 []))
          (map parse-lens in)))

(defn part2 [in]
  (apply + (mapcat (fn [[i box]]
                     (map-indexed (fn [slot [_ focal]] (apply * [(inc i) (inc slot) focal])) box))
                   (map-indexed vector (hash-boxes in)))))

(assert (= 145 (part2 (parse example))))
(assert (= 246762 (part2 (parse input))))
