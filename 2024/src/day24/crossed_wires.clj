(ns day24.crossed-wires
  (:require [clojure.string :as str]))

(def input (slurp "src/day24/input"))
(def example (slurp "src/day24/example"))
(def example2 (slurp "src/day24/example2"))

(defn parse [in]
  (let [[wires gates] (str/split in #"\n\n")]
    {:wires (into {} (map (fn [s] (let [[wire v] (str/split s #": ")]
                                   [wire (parse-long v)]))
                          (str/split-lines wires)))
     :gates (mapv rest (re-seq #"(.{3,3}) (AND|XOR|OR) (.{3,3}) -> (.{3,3})" gates))}))

(defn evaluate [wires [a op b c]]
  (let [av (get wires a)
        bv (get wires b)
        cv (get wires c)]
    (when (and (not cv) av bv)
      (let [r [c ((case op
                    "AND" bit-and
                    "XOR" bit-xor
                    "OR" bit-or) av bv)]]
        r))))

(defn run [{:keys [wires gates]}]
  (loop [gates gates wires wires]
    (if (empty? gates)
      wires
      (let [resolutions (keep (partial evaluate wires) gates)
            results (set (map first resolutions))]
        (recur (remove (fn [[_ _ _ out]] (contains? results out)) gates)
               (reduce (fn [acc [out v]] (assoc acc out v)) wires resolutions))))))

(defn part1 [in]
  (let [wires (run in)]
    (BigInteger. (apply str (reverse (for [d (range 64)
                                           :let [w (format "z%02d" d)
                                                 v (get wires w)]
                                           :when v]
                                       v)))
                 2)))

(assert (= 4 (part1 (parse example))))
(assert (= 2024 (part1 (parse example2))))
(assert (= 52038112429798 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
