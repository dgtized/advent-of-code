(ns aoc.day-11-monkey-in-the-middle
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

#_(aoc/day-input :day 11 :name "example" :content "")
#_(aoc/day-input :day 11)

{::clerk/visibility {:result :hide}}

(defn split-on-empty-line [file]
  (str/split (slurp file) #"\n\n"))

(defn parse-one-int [line]
  (parse-long (first (re-seq #"\d+" line))))

(defn parse-arg [arg]
  (when-not (= arg "old")
    (parse-long arg)))

(defn parse-monkey [lines]
  (let [[_ items operation & condition] (str/split-lines lines)
        [_ a op b] (re-find #"=\s([^\s]+)\s([\*\+])\s([^\s]+)" operation)
        [divisor on-true on-false] (mapv parse-one-int condition)]
    {:items (mapv parse-long (re-seq #"\d+" items))
     :op (fn [x] ((get {"*" * "+" +} op)
                 (or (parse-arg a) x)
                 (or (parse-arg b) x)))
     :divisor divisor
     :test (fn [x] (if (= 0 (mod x divisor)) on-true on-false))}))

(defn lcm [input]
  (apply * (mapv :divisor input)))

(defn parse [file]
  (mapv parse-monkey (split-on-empty-line file)))

(def example (parse "input/day11.example"))
(def e-mod (lcm example))

(def input (parse "input/day11.input"))
(def i-mod (lcm input))

(defn monkey [lcm input i]
  (let [{:keys [items op] :as monkey} (nth input i)]
    (-> (reduce (fn [input item]
                  (let [worry (if lcm
                                (mod (op item) lcm)
                                (int (/ (op item) 3)))
                        to ((:test monkey) worry)]
                    (update-in input [to :items] conj worry)))
                input items)
        (assoc-in [i :items] [])
        (assoc-in [i :inspections] (count items)))))

(defn round [lcm input]
  (reduce (partial monkey lcm) input (range (count input))))

(defn process [input rounds lcm]
  (->> (iterate (partial round lcm) input)
       rest
       (take rounds)
       (mapv (fn [step] (map :inspections step)))
       (reduce (partial mapv +))
       (sort >)
       (take 2)
       (apply *)))

(defn star1 [input]
  (process input 20 nil))

(defn star2 [input lcm]
  (process input 10000 lcm))

{::clerk/visibility {:result :show}}
(star1 example)
(star1 input)
(star2 example e-mod) ;; 2713310158
(star2 input i-mod)
