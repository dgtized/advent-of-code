(ns seven-segment-search
  (:require [clojure.string :as str]))

(defn parse [filename]
  (for [line (str/split-lines (slurp filename))]
    (map (fn [s] (str/split s #"\s+")) (str/split line #"\|\s+"))))

(defn part1 [input]
  (let [digits {2 1
                3 7
                4 4
                7 8}
        freqs (->> input
                   (map second)
                   (mapcat #(map (comp digits count) %))
                   frequencies)]
    (apply + (vals (dissoc freqs nil)))))

(assert (= 26 (part1 (parse "example"))))
(assert (= 352 (part1 (parse "input"))))

