(ns day03.gear-ratios
  (:require [clojure.string :as str]))

(def input (slurp "src/day03/input"))
(def example (slurp "src/day03/example"))

(defn region [s idx hit]
  (when s
    (subs s
          (max 0 (dec idx))
          (min (+ idx (inc (count hit)))
               (count s)))))

(defn adjacent [s]
  (str/replace s #"\d|\." ""))

(defn re-seq-idx [re s]
  (let [matcher (re-matcher re s)]
    ((fn step []
       (when-let [m (re-find matcher)]
         (cons [m (. matcher start)]
               (lazy-seq (step))))))))

(assert (= [["123" 0] ["234" 4]] (re-seq-idx #"\d+" "123.234")))

(defn check [[before line after]]
  (let [matcher (re-matcher #"\d+" line)]
    (loop [hits [] hit (re-find matcher)]
      (if hit
        (recur (conj hits
                     (let [idx (. matcher start)
                           r (str (region before idx hit)
                                  (region line idx hit)
                                  (region after idx hit))
                           a (adjacent r)]
                       [hit (= (count a) 1) r a]))
               (re-find matcher))
        hits))))

(defn check2 [[before line after]]
  (for [[_ idx] (re-seq-idx #"\*" line)]
    [idx
     (concat (keep (fn [[n i]]
                     (when (<= (- idx (count n)) i (inc idx))
                       [n i]))
                   (re-seq-idx #"\d+" before))
             (keep (fn [[n i]]
                     (when (or (= (- idx (count n)) i)
                               (= (inc idx) i))
                       [n i]))
                   (re-seq-idx #"\d+" line))
             (keep (fn [[n i]]
                     (when (<= (- idx (count n)) i (inc idx))
                       [n i]))
                   (re-seq-idx #"\d+" after)))]))

(defn parse [input check]
  (let [lines (str/split-lines input)
        groups (partition 3 1 lines)]
    (concat (check [nil (nth lines 0) (nth lines 1)])
            (mapcat check groups)
            (check [(nth lines (- (count lines) 2))
                    (nth lines (- (count lines) 1))
                    nil]))))

(defn part1 [parsed]
  (apply + (map (comp parse-long first) (filter (fn [[_ c]] c) parsed))))

(assert (= 4361 (part1 (parse example check))))
(assert (= 529618 (part1 (parse input check))))

;; (frequencies (map last (parse input check)))
;; (apply + (map (comp parse-long first) (parse input check)))

(defn part2 [parsed]
  (->> parsed
       (map second)
       (filter (fn [v] (= 2 (count v))))
       (map (fn [v] (apply * (map (comp parse-long first) v))))
       (apply +)))

(assert (= 467835 (part2 (parse example check2))))
(assert (= 77509019 (part2 (parse input check2))))


