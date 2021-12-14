(ns polymer
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [input (str/split-lines (slurp filename))]
    {:template (first input)
     :rules (->> (drop 2 input)
                 (map (fn [line]
                        (str/split line #" -> ")))
                 (reduce (fn [m [k v]]
                           (assoc m k v))
                         {}))}))

(defn apply-rules [rules template]
  (apply str (concat (map str
                          template
                          (for [[a b] (partition 2 1 template)]
                            (rules (str a b))))
                     [(last template)])))

(defn part1 [step {:keys [rules template]}]
  (let [result (last (take (inc step) (iterate (partial apply-rules rules) template)))
        freqs (vals (frequencies result))]
    [result (- (apply max freqs) (apply min freqs))]))

(assert (= "NCNBCHB" (first (part1 1 (parse "example")))))
(assert (= "NBCCNBBBCBHCB" (first (part1 2 (parse "example")))))
(assert (= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" (first (part1 4 (parse "example")))))
(assert (= 1588 (second (part1 10 (parse "example")))))

(assert (= 2703 (second (part1 10 (parse "input")))))

(defn expansions [rules]
  (into {}
        (for [[k v] rules]
          (let [[a b] k]
            [k [(str a v) (str v b)]]))))

(defn sum-of-maps [maps]
  (apply merge-with + maps))

(defn count-expansions [exp-rules freqs]
  (->> freqs
       (mapcat (fn [[rule n]]
                 (for [exp (exp-rules rule)]
                   {exp n})))
       sum-of-maps))

(defn part2 [step {:keys [rules template]}]
  (let [template-freqs (->> template
                            (partition 2 1)
                            (map (partial apply str))
                            frequencies)
        counts (nth (iterate (partial count-expansions (expansions rules)) template-freqs) step)
        freqs (-> (for [[e n] counts]
                    {(first e) n})
                  (into
                   [{(last template) 1}])
                  sum-of-maps)
        values (sort (vals freqs))]
    [freqs (- (last values) (first values))]))

(assert (= 1588 (second (part2 10 (parse "example")))))
(assert (= 2188189693529 (second (part2 40 (parse "example")))))
(assert (= 2984946368465 (second (part2 40 (parse "input")))))

;; various failed attempts to expand the rules instead of the template or solve the expansion
(comment
  (defn pair-expansion [step pair rules]
    (take (inc step) (iterate (partial apply-rules rules) pair)))

  (let [{:keys [rules]} (parse "example")]
    (for [form ["NN" "NC" "CB"]]
      (time (frequencies (last (pair-expansion 10 form rules))))))

  (let [{:keys [rules]} (parse "example")]
    (for [form ["NN" "NC" "CB"]]
      (map (comp (partial sort-by first) frequencies) (pair-expansion 6 form rules))))

  (defn step-rules [{:keys [rules]}]
    (for [match (keys rules)
          :let [freqs (frequencies (last (take 11 (iterate (partial apply-rules rules) match))))]]
      [match freqs (- (get freqs \B 0) (get freqs \H 0))]))

  (step-rules (parse "example"))
  (step-rules (parse "input"))

  (defn best-worst [step {:keys [rules template]}]
    (let [result (nth (iterate (partial apply-rules rules) template) step)
          freqs (sort-by second (frequencies result))]
      [(last freqs) (first freqs)]))
  (best-worst 10 (parse "example"))
  (best-worst 10 (parse "input")))

