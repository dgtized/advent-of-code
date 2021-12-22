(ns reactor-reboot
  (:require [clojure.string :as str]))

(defn extract-range [s]
  (mapv parse-long (str/split s #"\.\.")))

(defn parse [filename]
  (mapv (fn [line] (let [[value & coords] (rest (re-find #"(on|off) x=(.*),y=(.*),z=(.*)$" line))]
                    [(mapv extract-range coords) (= "on" value)]))
        (str/split-lines (slurp filename))))

(defn inside-50? [x y z]
  (and (<= -50 x 50)
       (<= -50 y 50)
       (<= -50 z 50)))

(defn expand [[[[x0 x1] [y0 y1] [z0 z1]] toggle]]
  (for [x (range (max x0 -50) (min 51 (inc x1)))
        y (range (max y0 -50) (min 51 (inc y1)))
        z (range (max z0 -50) (min 51 (inc z1)))]
    [[x y z] toggle]))

(assert (= 27 (count (expand [[[10 12] [10 12] [10 12]] true]))))

(defn part1 [input]
  (reduce (fn [s cube]
            (reduce
             (fn [s2 [c v]] (if v (assoc s2 c v) (dissoc s2 c))) s (expand cube)))
          {} input))

(assert (= 39 (time (count (part1 (parse "example"))))))
(assert (= 590784 (time (count (part1 (parse "example1"))))))
(assert (= 582644 (time (count (part1 (parse "input"))))))

