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

(defn step [[grid _]]
  (let [east-moves (->> grid
                        (facing \>)
                        (keep (partial build-move east grid)))
        grid-east (apply-moves grid east-moves)
        south-moves (->> grid-east
                         (facing \v)
                         (keep (partial build-move south grid-east)))
        grid' (apply-moves grid-east south-moves)]
    [grid' (+ (count east-moves) (count south-moves))]))

(assert (= 58 (count (take-while (fn [[g steps]] (pos? steps)) (iterate step [(parse "example") 1])))))
(assert (= 360 (count (take-while (fn [[g steps]] (pos? steps)) (iterate step [(parse "input") 1])))))

