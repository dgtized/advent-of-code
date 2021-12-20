(ns trench-map
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [[enhance & image] (str/split-lines (slurp filename))]
    {:enhance enhance
     :image (rest image)}))

(def char->int {\. 0 \# 1})

(defn image->grid [{:keys [image]}]
  (into {} (for [[j line] (map-indexed vector image)
                 [i character] (map-indexed vector (seq line))]
             [[i j] (char->int character)])))

(assert (= {0 15 1 10} (frequencies (vals (image->grid (parse "example"))))))

(defn pixel-offset [image [i j]]
  (let [bits (for [dj [-1 0 1]
                   di [-1 0 1]]
               (get image (mapv + [i j] [dj di]) 0))]
    (Integer/parseInt (apply str bits) 2)))

(assert (= 456 (pixel-offset (image->grid (parse "example")) [1 1])))
(assert (= 1 (char->int (nth (:enhance (parse "example")) 34))))

