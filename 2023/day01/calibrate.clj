(ns calibrate
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn part1 []
  (->> (slurp "day01/input")
       str/split-lines
       (map (fn [line] (keep (set (map (fn [d] (char (+ (byte \0) d))) (range 10))) (vec line))))
       (map (juxt first last))
       (map (fn [x] (edn/read-string (apply str x))))
       (reduce +)))

(comment
  (part1) ;; => 55447
  )

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
       (map (fn [x] (edn/read-string (apply str x))))
       (reduce +)
       ))

(comment
  (part2 example) ;; => 281
  (part2 (slurp "day01/input")) ;; => 54706
  )
