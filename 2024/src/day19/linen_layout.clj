(ns day19.linen-layout
  (:require [clojure.string :as str]))

(def input (slurp "src/day19/input"))
(def example (slurp "src/day19/example"))

(defn parse [in]
  (let [[available patterns] (str/split in #"\n\n")]
    {:towels (str/split available #", ")
     :patterns (str/split-lines patterns)}))

(defn match [towels]
  (let [available (str/join "|" towels)
        p (re-pattern (str "(" available ")+"))]
    (fn [pattern]
      (re-matches p pattern))))

(defn part1 [{:keys [towels patterns]}]
  (count (keep (match towels) patterns)))

(assert (= 6 (part1 (parse example))))
(assert (= 265 (part1 (parse input))))

(defn subset [towels pattern]
  (keep (fn [towel]
          (when (str/starts-with? pattern towel)
            [towel (str/replace-first pattern towel "")]))
        towels))

(let [{:keys [towels patterns]} (parse example)]
  (map (partial subset towels) patterns))

(defn expand [towels pattern]
  (cond (empty? pattern)
        [[]]
        :else
        (when-let [m (seq (subset towels pattern))]
          (mapcat (fn [[token remaining]]
                    (when-let [expansions (seq (expand towels remaining))]
                      (keep (fn [p] (when p (cons token p)))
                            expansions)))
                  m))))

(assert (= [["a" "b"] ["ab"]] (expand ["a" "ab" "b"] "ab")))
(assert (= [["a" "ab"]] (expand ["a" "ab"] "aab")))

(defn part2 [{:keys [towels patterns]}]
  (apply + (map count (map (fn [pattern] (println pattern)
                             (expand towels pattern)) patterns))))

(assert (= 16 (part2 (parse example))))
;; (println (part2 (parse input)))
;; (assert (= (part2 (parse input))))
