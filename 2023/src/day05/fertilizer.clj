(ns day05.fertilizer
  (:require [clojure.string :as str]))

(def input (slurp "src/day05/input"))
(def example (slurp "src/day05/example"))

(defn remap [x [out in len]]
  (when (<= in x (+ in len))
    (+ out (- x in))))

(assert (= 51 (remap 99 [50 98 2])))

(defn parse-nums [s]
  (mapv parse-long (re-seq #"\d+" s)))

(defn parse [in]
  (let [sections (str/split in #"\n\n")]
    {:seeds (parse-nums (first sections))
     :remaps (vec (for [section (map str/split-lines (rest sections))]
                    [(vec (rest (re-find #"(\w+)-to-(\w+)\s" (first section))))
                     (mapv parse-nums (rest section))]))}))

(defn thread [seed remaps]
  (reduce (fn [ids [from-to maps]]
            (let [id (last ids)]
              (conj ids (if-let [m (some (partial remap id) maps)]
                          m
                          id))))
          [seed]
          remaps))

(defn part1 [in]
  (->> (let [{:keys [seeds remaps]} (parse in)]
         (for [seed seeds]
           (thread seed remaps)))
       (apply min-key last)
       last))

(defn thread-view1 [in]
  (->> (let [{:keys [seeds remaps]} (parse in)]
         (for [seed seeds]
           (thread seed remaps)))))

(thread-view1 input)

(assert (= 35 (part1 example)))
(assert (= 379811651 (part1 input)))

(defn bruteforce-part2 [in]
  (->> (let [{:keys [seeds remaps]} (parse in)]
         (mapcat (fn [[start len]]
                   (mapv #(thread % remaps) (range start (+ start len))))
                 (partition 2 2 seeds)))
       (apply min-key last)
       last))

(assert (= 46 (bruteforce-part2 example)))
;; (assert (= (part2 input)))

;; [3,6] => [0,6]
;; [0,6] => [3,6]
(defn input-remap [x [out in len]]
  (when (<= out x (+ out len))
    (+ in (- x out))))

;; (input-remap 1 [1 3 6])

(defn range-intersect [[a a-len] [b b-len]]
  (when-not (or (and (< a b) (<= (+ a a-len) b))
                (and (< b a) (<= (+ b b-len) a)))
    (if (< a b)
      [b (- (min (+ a a-len) (+ b b-len)) b)]
      [a (- (min (+ a a-len) (+ b b-len)) a)])))

(assert (nil? (range-intersect [2 3] [7 8])))
(assert (nil? (range-intersect [7 8] [2 3])))
(assert (= [3 3] (range-intersect [3 3] [3 3])))
(assert (= [4 2] (range-intersect [3 3] [4 4])))
(assert (= [4 1] (range-intersect [2 3] [4 2])))
(assert (= [4 4] (range-intersect [4 4] [3 7])))

(defn reverse-remap [[x x-len] [out in len]]
  (when-let [[o l] (range-intersect [x x-len] [out len])]
    (let [s (input-remap o [out in len])]
      [s (- (input-remap (+ o l) [out in len]) s)])))

(assert (= [10 10] (reverse-remap [0 10] [0 10 10])))
(assert (= [10 5] (reverse-remap [0 10] [5 10 10])))
(assert (= [10 10] (reverse-remap [5 10] [5 10 10])))
(assert (= [15 5] (reverse-remap [10 10] [5 10 10])))
(reverse-remap [56 37] [1 0 69]) ;; [55 14]

(map (fn [r] (reverse-remap [10 10] r))
     [[10 0 10]
      [5 0 10]
      [0 5 10]
      [0 5 15]
      [0 10 10]])

(sort-by (juxt first second) [[5 2] [4 3] [2 8] [5 3] [4 4] [4 2]])

(defn reverse-part2 [in]
  (let [{:keys [remaps seeds]} (parse in)
        reversed (mapv second (reverse remaps))]
    {:seed-ranges (partition 2 2 seeds)
     :remaps reversed
     :traces
     (reductions (fn [ranges maps]
                   (mapcat
                    (fn [r] (keep (fn [m] (reverse-remap r m)) maps))
                    ranges))
                 (mapv (fn [[o _ l]] [o l]) (first reversed))
                 reversed)}))

(reverse-part2 example)
;; (reverse-part2 input)

(defn remap-range [[x x-len] [out in len]]
  (when-let [[o l] (range-intersect [x x-len] [in len])]
    (let [s (remap o [out in len])]
      [s (- (remap (+ o l) [out in len]) s)])))

(defn ordered [coll]
  (dedupe (sort-by (juxt first second) coll)))

(defn forward-part2 [in]
  (let [{:keys [seeds remaps]} (parse in)]
    {:seed-ranges (partition 2 2 seeds)
     :remaps remaps
     :traces
     (->> (reduce (fn [ranges maps]
                    (ordered
                     (mapcat
                      (fn [r] (if-let [s (seq (keep (fn [m] (remap-range r m)) maps))]
                               s
                               [r]))
                      ranges)))
                  (partition 2 2 seeds)
                  (map second remaps))
          ordered)}))

(forward-part2 example)
;; (:traces (forward-part2 input))
