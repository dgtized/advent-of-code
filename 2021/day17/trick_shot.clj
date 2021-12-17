(ns trick-shot
  (:require [clojure.string :as str]))

(def example "target area: x=20..30, y=-10..-5")
(def input "target area: x=135..155, y=-102..-78")

(defn parse [input]
  (mapv (fn [s] (mapv parse-long (str/split s #"\.\.")))
        (rest (re-find #"x=([^,]+), y=(.+)$" input))))

;; (mapv parse [example input])

(defn probe [dx dy]
  {:x 0 :y 0 :dx dx :dy dy})

(defn probe-step [{:keys [x y dx dy] :as probe}]
  (assoc probe
         :x (+ x dx)
         :y (+ y dy)
         :dx (cond (> dx 0) (dec dx)
                   (< dx 0) (inc dx)
                   :else 0)
         :dy (dec dy)))

(defn possible? [{:keys [x y]} [[x0 x1] [y0 y1]]]
  (and (<= x x1) (>= y y0)))

(defn hit? [{:keys [x y]} [[x0 x1] [y0 y1]]]
  (and (<= x0 x x1)
       (<= y0 y y1)))

(defn trajectory [dx dy target]
  (take-while (fn [p] (possible? p target))
              (iterate probe-step (probe dx dy))))

(defn summarize [dx dy target]
  (let [path (trajectory dx dy target)]
    (when (hit? (last path) target)
      [[dx dy] (apply max (map :y path))])))

(summarize 7 2 (parse example))
(summarize 6 3 (parse example))
(summarize 6 9 (parse example))
(summarize 9 0 (parse example))
(summarize 17 -4 (parse example))

(defn search [target]
  (keep (fn [[dx dy]] (summarize dx dy target))
        (for [dx (range 0 250)
              dy (range 0 300)]
          [dx dy])))

(assert (= [[7 9] 45] (apply max-key second (search (parse example)))))
(assert (= [[17 101] 5151]) (apply max-key second (search (parse input))))


