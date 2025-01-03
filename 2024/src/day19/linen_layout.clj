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

(defn expansions [cache towels pattern]
  (if-let [r (get @cache pattern)]
    r
    (let [r (if (empty? pattern)
              1
              (if-let [m (seq (subset towels pattern))]
                (apply + (map (fn [[_ remaining]]
                                (expansions cache towels remaining))
                              m))
                0))]
      (swap! cache assoc pattern r)
      r)))

(assert (= [["a" "b"] ["ab"]] (expand ["a" "ab" "b"] "ab")))
(assert (= [["a" "ab"]] (expand ["a" "ab"] "aab")))
(assert (= 2 (expansions (atom {}) ["a" "ab" "b"] "ab")))
(assert (= 1 (expansions (atom {}) ["a" "ab"] "aab")))

(defn part2 [{:keys [towels patterns]}]
  (apply + (map (fn [pattern] (expansions (atom {}) towels pattern))
                patterns)))

(assert (= 16 (part2 (parse example))))
(assert (= 752461716635602 (part2 (parse input))))
