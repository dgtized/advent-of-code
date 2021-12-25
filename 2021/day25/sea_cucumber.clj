(ns sea-cucumber
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [grid (into {} (for [[j line] (map-indexed vector (str/split-lines (slurp filename)))
                            [i c] (map-indexed vector line)]
                        [[i j] c]))]
    (assoc grid
           :width (inc (apply max (mapv first (keys grid))))
           :height (inc (apply max (mapv second (keys grid)))))))

(defn south [{:keys [height]} [i j]]
  (if (= j (dec height))
    [i 0]
    [i (inc j)]))

(defn east [{:keys [width]} [i j]]
  (if (= i (dec width))
    [0 j]
    [(inc i) j]))

(defn build-move [direction grid pos]
  (let [p (direction grid pos)]
    (when (= \. (get grid p))
      [pos p])))

(defn facing [face grid]
  (keep (fn [[p c]] (when (= c face) p)) grid))

(assert (seq (facing \> (parse "example"))))

(defn apply-moves [grid moves]
  (reduce (fn [g [a b]]
            (assoc g
                   a \.
                   b (get grid a)))
          grid moves))

(defn step [{:keys [grid]}]
  (let [east-moves (keep (partial build-move east grid)
                         (facing \> grid))
        grid-east (apply-moves grid east-moves)
        south-moves (keep (partial build-move south grid-east)
                          (facing \v grid-east))]
    {:grid (apply-moves grid-east south-moves)
     :steps (+ (count east-moves) (count south-moves))}))

(defn count-steps [grid]
  (->> {:grid grid :steps 1}
       (iterate step)
       (take-while (fn [{:keys [steps]}] (pos? steps)))
       count))

(assert (= 58 (count-steps (parse "example"))))
(assert (= 360 (count-steps (parse "input"))))

