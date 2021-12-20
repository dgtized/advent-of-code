(ns trench-map
  (:require [clojure.string :as str]))

(defn parse [filename]
  (let [[algorithm & image] (str/split-lines (slurp filename))]
    {:algorithm algorithm
     :image (rest image)}))

(defn image->grid [{:keys [image]}]
  (into {} (for [[j line] (map-indexed vector image)
                 [i character] (map-indexed vector (seq line))]
             [[i j] (get {\. 0 \# 1} character)])))

(assert (= {0 15 1 10} (frequencies (vals (image->grid (parse "example"))))))
