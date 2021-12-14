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

(defn apply-rules [{:keys [template rules]}]
  {:template (apply str (concat (map str
                                     template
                                     (for [[a b] (partition 2 1 template)]
                                       (rules (str a b))))
                                [(last template)]))
   :rules rules})

(defn part1 [step input]
  (let [{:keys [template]} (last (take (inc step) (iterate apply-rules input)))
        freqs (vals (frequencies template))]
    [template (- (apply max freqs) (apply min freqs))]))

(assert (= "NCNBCHB" (first (part1 1 (parse "example")))))
(assert (= "NBCCNBBBCBHCB" (first (part1 2 (parse "example")))))
(assert (= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" (first (part1 4 (parse "example")))))
(assert (= 1588 (second (part1 10 (parse "example")))))

(assert (= 2703 (second (part1 10 (parse "input")))))

(defn part2 [step input]
  (take (inc step) (map (comp frequencies :template) (iterate apply-rules input))))

;; (part2 16 (parse "example"))
(take-last 2 (part2 12 (parse "input")))

;; (for [i (range 1 20)]
;;   (time (second (part1 i (parse "example")))))

(defn app-rule [rules template]
  (apply str (concat (map str
                          template
                          (for [[a b] (partition 2 1 template)]
                            (rules (str a b))))
                     [(last template)])))

(defn pair-expansion [step pair rules]
  (take (inc step) (iterate (partial app-rule rules) pair)))

(let [{:keys [rules]} (parse "example")]
  (for [form ["NN" "NC" "CB"]]
    (pair-expansion 6 form rules)))

(let [{:keys [rules]} (parse "example")]
  (for [form ["NN" "NC" "CB"]]
    (map (comp (partial sort-by first) frequencies) (pair-expansion 6 form rules))))

(defn step-rules [{:keys [rules]}]
  (for [[match expansion] rules
        :let [result1 (apply str (interpose expansion match))
              result2 (:template (apply-rules {:template result1 :rules rules}))
              result3 (:template (apply-rules {:template result2 :rules rules}))
              result4 (:template (apply-rules {:template result3 :rules rules}))]]
    [match result1 result2 result3 result4 (frequencies result3) (frequencies result4)]))

(comment
  (step-rules (parse "example"))
  (step-rules (parse "input")))

;; (time (second (part1 32 (parse "example"))))
;; (assert (= 2188189693529 (second (part1 40 (parse "example")))))

