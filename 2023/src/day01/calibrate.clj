(ns day01.calibrate
  (:require
   [clojure.string :as str]))

(def input (slurp "src/day01/input"))

(defn part1 [input]
  (->> input
       str/split-lines
       (map (fn [line] (keep (set (map (fn [d] (char (+ (byte \0) d))) (range 10))) (vec line))))
       (map (juxt first last))
       (map (fn [x] (parse-long (apply str x))))
       (apply +)))

(assert (= 55447 (part1 input)))

(def example "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(def patterns
  {#"^(1|one)" 1
   #"^(2|two)" 2
   #"^(3|three)" 3
   #"^(4|four)" 4
   #"^(5|five)" 5
   #"^(6|six)" 6
   #"^(7|seven)" 7
   #"^(8|eight)" 8
   #"^(9|nine)" 9})

(defn part2 [input]
  (->> input
       str/split-lines
       (map (fn [line]
              (keep (fn [in]
                      (some (fn [[pattern val]]
                              (when (re-find pattern in)
                                val))
                            patterns))
                    (take-while (fn [s] (> (count s) 0))
                                (iterate (fn [s] (subs s 1)) line)))))
       (map (juxt first last))
       (map (fn [x] (parse-long (apply str x))))
       (apply +)))

(assert (= 281 (part2 example)))
(assert (= 54706 (part2 input)))

