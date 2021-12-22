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
  (for [x (range x0 (inc x1))
        y (range y0 (inc y1))
        z (range z0 (inc z1))
        :when (inside-50? x y z)]
    [[x y z] toggle]))

(assert (= 27 (count (expand [[[10 12] [10 12] [10 12]] true]))))

(defn count-on [grid]
  (reduce-kv (fn [acc k v] (if v (inc acc) acc))
             0 grid))

(defn part1 [input]
  (count-on (reduce (fn [s cube]
                      (reduce
                       (fn [s2 [c v]] (assoc s2 c v)) s (expand cube)))
                    {} input)))

(part1 (parse "example1"))

