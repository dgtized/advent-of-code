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

