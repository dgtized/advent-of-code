(ns day09.disk-fragmenter
  (:require [clojure.string :as str]))

(def input (slurp "src/day09/input"))
(def example (slurp "src/day09/example"))

(defn parse [in]
  (map (comp parse-long str) (str/trimr in)))

(defn expand [layout]
  (remove (fn [{:keys [gap]}]  (= gap 0))
          (mapcat (fn [[idx [len gap]]]
                    (concat [{:idx idx :len len}]
                            (when gap [{:gap gap}])))
                  (map-indexed vector (partition-all 2 2 layout)))))

(defn defrag [chunks]
  (loop [chunks (vec chunks) fs []]
    (if (seq chunks)
      (let [chunk (first chunks)]
        (cond (pos? (get chunk :len 0))
              (recur (rest chunks) (conj fs chunk))
              (or (zero? (:gap chunk)) (empty? (rest chunks)))
              (recur (rest chunks) fs)
              :else
              (let [gap (get chunk :gap 0)
                    lc (nth chunks (dec (count chunks)))
                    n (if (:gap lc) 2 1)
                    {lidx :idx llen :len} (if (= n 1) lc (nth chunks (- (count chunks) 2)))]
                (recur (concat
                        (when (> gap llen)
                          [{:gap (- gap llen)}])
                        (vec (rest (drop-last n chunks)))
                        (when (< gap llen)
                          [{:idx lidx
                            :len (- llen gap)}]))
                       (conj fs {:idx lidx :len (min gap llen)})))))
      fs)))

(defn checksum [fs]
  (loop [fs fs i 0 sum 0]
    (if (seq fs)
      (let [{:keys [idx len]} (first fs)]
        (recur (rest fs)
               (+ i len)
               (+ sum (apply + (map (partial * idx) (range i (+ i len)))))))
      sum)))

(comment (defrag (expand (parse "12345")))
         (defrag (expand (parse example))))
#_(defn defrag [layout]
    (loop [idx 0
           layout layout
           file-idx 0
           last-file-idx (int (/ (dec (count layout)) 2))
           checksum 0]
      (let [[len gap] (take 2 layout)]
        (recur (+ checksum (calc-checksum idx file-idx len))))))

(defn part1 [in]
  (checksum (defrag (expand in))))

(assert (= 1928 (part1 (parse example))))
;; slow 6146.933911 msecs / 9s with gap drop
(time (assert (= 6607511583593 (part1 (parse input)))))

(comment (expand (parse input)))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
