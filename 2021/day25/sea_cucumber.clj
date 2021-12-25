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

(defn test-move [grid direction pos]
  (let [p (direction grid pos)]
    (when (= \. (get grid p))
      [pos p])))

(defn move [grid a b]
  (assoc grid
         a \.
         b (get grid a)))

(defn facing [face grid]
  (keep (fn [[p c]] (when (= c face) p)) grid))

(assert (seq (facing \> (parse "example"))))

(defn grid-update [grid moves]
  (reduce (fn [g [a b]] (move g a b))
          grid moves))

(defn step [grid]
  (let [east-moves (->> grid
                        (facing \>)
                        (keep (partial test-move east)))
        grid-east (grid-update grid east-moves)
        south-moves (->> grid-east
                         (facing \v)
                         (keep (partial test-move south)))
        grid' (grid-update grid-east south-moves)]
    grid'))

(parse "example")
