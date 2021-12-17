(ns trick-shot
  (:require [clojure.string :as str]))

(def example "target area: x=20..30, y=-10..-5")
(def input "target area: x=135..155, y=-102..-78")

(defn parse [input]
  (mapv (fn [s] (mapv parse-long (str/split s #"\.\.")))
        (rest (re-find #"x=([^,]+), y=(.+)$" input))))

;; (mapv parse [example input])

(defn make-probe [dx dy]
  {:x 0 :y 0 :dx dx :dy dy})

(defn probe-step [{:keys [x y dx dy] :as probe}]
  (assoc probe
         :x (+ x dx)
         :y (+ y dy)
         :dx (cond (> dx 0) (dec dx)
                   (< dx 0) (inc dx)
                   :else 0)
         :dy (dec dy)))

(defn in-bounds? [[[_ x1] [y0 _]] {:keys [x y]}]
  (and (<= x x1) (>= y y0)))

(defn hit? [[[x0 x1] [y0 y1]] {:keys [x y]}]
  (and (<= x0 x x1) (<= y0 y y1)))

(defn trajectory [probe target]
  (take-while (partial in-bounds? target)
              (iterate probe-step probe)))

(defn summarize [{:keys [dx dy] :as probe} target]
  (let [path (trajectory probe target)]
    (when (hit? target (last path))
      [[dx dy] (apply max (map :y path))])))

(assert (summarize (make-probe 7 2) (parse example)))
(assert (summarize (make-probe 6 3) (parse example)))
(assert (summarize (make-probe 6 9) (parse example)))
(assert (summarize (make-probe 9 0) (parse example)))
(assert (not (summarize (make-probe 17 -4) (parse example))))

(defn search-max [target]
  (keep (fn [probe] (summarize probe target))
        (for [dx (range 0 250)
              dy (range 0 300)]
          (make-probe dx dy))))

(assert (= [[7 9] 45] (apply max-key second (search-max (parse example)))))
(assert (= [[17 101] 5151] (apply max-key second (search-max (parse input)))))

(defn search-all [target]
  (keep (fn [probe] (summarize probe target))
        (for [dx (range 0 175)
              dy (range -125 125)]
          (make-probe dx dy))))

(assert (= 112 (count (search-all (parse example)))))
(assert (= 968 (count (search-all (parse input)))))
