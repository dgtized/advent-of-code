(ns aoc.day-10
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

#_(aoc/day-input :day 10 :name "example" :content "")
#_(aoc/day-input :day 10)

{::clerk/visibility {:result :hide}}

(def i-ex "input/day10.example")
(def i-ex2 "input/day10.example2")

(defn parse [file]
  (for [line (aoc/file->lines file)]
    (cond (str/starts-with? line "noop")
          [:noop 0 1]
          (str/starts-with? line "addx")
          (let [[op v] (str/split line #"\s")]
            [(keyword op) (parse-long v) 2]))))

(defn interpret [input]
  (loop [{:keys [reg] :as s}
         {:cycle 1 :reg 1}
         cmds (concat input [[:noop 0 1] [:noop 0 1] [:noop 0 1]])
         steps []]
    (if (empty? cmds)
      steps
      (let [[cmd v c] (first cmds)]
        (recur (-> s
                   (assoc :reg (+ reg v))
                   (update :cycle + c))
               (rest cmds)
               (into steps
                     (let [s' (assoc s :cmd [cmd v])]
                       (if (= cmd :noop)
                         [s']
                         [s' (update s :cycle inc)]))))))))

(interpret (parse i-ex))
(take 30 (interpret (parse i-ex2)))

(defn star1 [file]
  (let [offsets [20 60 100 140 180 220]
        states (interpret (parse file))]
    (->> offsets
         (map #(nth states (dec %)))
         (map (fn [{:keys [cycle reg]}] (* cycle reg)))
         (apply +))))

#_(interpret (parse "input/day10.input"))
#_(star1 "input/day10.input")

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (remove #{"input/day10.example2"} (aoc/input-files "day10"))
 (fn [{:keys [result]}] result))
