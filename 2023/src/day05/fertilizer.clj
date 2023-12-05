(ns day05.fertilizer
  (:require [clojure.string :as str]))

(def input (slurp "src/day05/input"))
(def example (slurp "src/day05/example"))

(defn remap [x [out in len]]
  (when (<= in x (+ in len))
    (+ out (- x in))))

(assert (= 51 (remap 99 [50 98 2])))

(defn parse-nums [s]
  (mapv parse-long (re-seq #"\d+" s)))

(defn parse [in]
  (let [sections (str/split in #"\n\n")]
    {:seeds (parse-nums (first sections))
     :remaps (vec (for [section (map str/split-lines (rest sections))]
                    [(vec (rest (re-find #"(\w+)-to-(\w+)\s" (first section))))
                     (mapv parse-nums (rest section))]))}))

(defn thread [seed remaps]
  (reduce (fn [ids [from-to maps]]
            (let [id (last ids)]
              (conj ids (if-let [m (some (partial remap id) maps)]
                          m
                          id))))
          [seed]
          remaps))

(defn part1 [in]
  (->> (let [{:keys [seeds remaps]} (parse in)]
         (for [seed seeds]
           (thread seed remaps)))
       (apply min-key last)
       last))

(assert (= 35 (part1 example)))
(assert (= 379811651 (part1 input)))
