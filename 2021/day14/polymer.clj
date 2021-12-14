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

;; (for [i (range 1 20)]
;;   (time (second (part1 i (parse "example")))))

(defn pair-expansion [step pair rules]
  (take (inc step) (iterate (partial apply-rules rules) pair)))

(let [{:keys [rules]} (parse "example")]
  (for [form ["NN" "NC" "CB"]]
    (pair-expansion 6 form rules)))

(let [{:keys [rules]} (parse "example")]
  (for [form ["NN" "NC" "CB"]]
    (map (comp (partial sort-by first) frequencies) (pair-expansion 6 form rules))))

(defn step-rules [{:keys [rules]}]
  (for [[match expansion] rules
        :let [result1 (apply str (interpose expansion match))
              result2 (apply-rules rules result1)
              result3 (apply-rules rules result2)
              result4 (apply-rules rules result3)]]
    [match result1 result2 result3 result4 (frequencies result3) (frequencies result4)]))

(comment
  (step-rules (parse "example"))
  (step-rules (parse "input")))

;; (time (second (part1 32 (parse "example"))))
;; (assert (= 2188189693529 (second (part1 40 (parse "example")))))


(let [{:keys [template rules]} (parse "input")]
  (for [form (take 2 (partition 2 1 template))]
    (map (comp (partial sort-by second) frequencies) (pair-expansion 10 form rules))))

(defn best-worst [step {:keys [rules template]}]
  (let [result (last (take (inc step) (iterate (partial apply-rules rules) template)))
        freqs (sort-by second (frequencies result))]
    [(last freqs) (first freqs)]))

(best-worst 10 (parse "example"))
(best-worst 10 (parse "input"))
