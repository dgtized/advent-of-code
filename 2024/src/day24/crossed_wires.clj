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


(defn wire [prefix digit]
  (format "%s%02d" prefix digit))

(defn part1 [in]
  (let [wires (run in)]
    (BigInteger. (apply str (reverse (for [d (range 64)
                                           :let [w (wire "z" d)
                                                 v (get wires w)]
                                           :when v]
                                       v)))
                 2)))

(assert (= 4 (part1 (parse example))))
(assert (= 2024 (part1 (parse example2))))
(assert (= 52038112429798 (part1 (parse input))))

(defn calc [in]
  (let [wires (run in)]
    (apply str (reverse (for [d (range 64)
                              :let [w (wire "z" d)
                                    v (get wires w)]
                              :when v]
                          v)))))

(defn part2 [in]
  (let [orig (run in)]
    (for [d (range 64)
          :let [x-wire (wire "x" d)
                y-wire (wire "y" d)
                z-wire (wire "z" d)]
          :when (and (get-in in [:wires x-wire]))
          :let [place (if (zero? d)
                        in
                        (-> in
                            (assoc-in [:wires x-wire] 0)
                            (assoc-in [:wires y-wire] 0)))]]
      [z-wire (for [var [x-wire y-wire]
                    :let [v (get-in in [:wires var])]
                    :when v
                    :let [r (run (assoc-in place [:wires var] 0))
                          r' (run (assoc-in place [:wires var] 1))]]
                [var
                 [(get r (wire "z" (inc d))) (get r z-wire)]
                 [(get r' (wire "z" (inc d))) (get r' z-wire)]])])))

(assert (= (part2 (parse example))))
(assert (= (part2 (parse example2))))
(assert (= (part2 (parse input))))
