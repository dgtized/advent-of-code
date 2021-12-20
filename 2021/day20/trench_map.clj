(ns trench-map
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [[enhance & image] (str/split-lines (slurp filename))]
    {:enhance enhance
     :image (rest image)}))

(def char->int {\. 0 \# 1})
(def int->char {0 \. 1 \#})

(defn image->grid [{:keys [image]}]
  (into {} (for [[j line] (map-indexed vector image)
                 [i character] (map-indexed vector (seq line))]
             [[i j] (char->int character)])))

(assert (= {0 15 1 10} (frequencies (vals (image->grid (parse "example"))))))

(defn pixel-offset [image [i j]]
  (let [bits (for [dj [-1 0 1]
                   di [-1 0 1]]
               (get image (mapv + [i j] [di dj]) 0))]
    (Integer/parseInt (apply str bits) 2)))

(assert (= 294 (pixel-offset (image->grid (parse "example")) [1 1])))
(assert (= 1 (char->int (nth (:enhance (parse "example")) 34))))

(defn bounds [image]
  (let [coords (keys image)]
    (for [axis [first second]]
      [(apply min (mapv axis coords))
       (inc (apply max (mapv axis coords)))])))

(defn convolve [enhance image]
  (let [[[x0 x1] [y0 y1]] (bounds image)]
    (into {}
          (for [i (range (- x0 1) (+ x1 1))
                j (range (- y0 1) (+ y1 1))
                :let [value (char->int (nth enhance (pixel-offset image [i j])))]
                :when (= value 1)]
            [[i j] value]))))

(defn part1 [{:keys [enhance] :as input} steps]
  (-> (iterate (partial convolve enhance) (image->grid input))
      (nth steps)))

(defn print-grid [image]
  (let [[[x0 x1] [y0 y1]] (bounds image)]
    (mapv (partial apply str)
          (partition (- (+ x1 3) (- x0 3))
                     (for [j (range (- y0 3) (+ y1 3))
                           i (range (- x0 3) (+ x1 3))]
                       (int->char (get image [i j] 0)))))))

(comment (print-grid (part1 (parse "example") 2))
         (print-grid (part1 (parse "input") 0))
         (print-grid (part1 (parse "input") 1))
         (print-grid (part1 (parse "input") 2))
         (print-grid (part1 (parse "input") 3))
         )
(assert (= 35 ((frequencies (vals (part1 (parse "example") 2))) 1)))
((frequencies (vals (image->grid (parse "input")))) 1)
((frequencies (vals (part1 (parse "input") 0))) 1)
((frequencies (vals (part1 (parse "input") 1))) 1)
((frequencies (vals (part1 (parse "input") 2))) 1)

