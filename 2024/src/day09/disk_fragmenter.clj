(ns day09.disk-fragmenter
  (:require [clojure.string :as str]))

(def input (slurp "src/day09/input"))
(def example (slurp "src/day09/example"))

(defn parse [in]
  (map (comp parse-long str) (str/trimr in)))

(defn expand [layout]
  (remove (fn [{:keys [gap]}]  (= gap 0))
          (mapcat (fn [[id [len gap]]]
                    (concat [{:id id :len len}]
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
                    {lid :id llen :len} (if (= n 1) lc (nth chunks (- (count chunks) 2)))]
                (recur (concat
                        (when (> gap llen)
                          [{:gap (- gap llen)}])
                        (vec (rest (drop-last n chunks)))
                        (when (< gap llen)
                          [{:id lid
                            :len (- llen gap)}]))
                       (conj fs {:id lid :len (min gap llen)})))))
      fs)))

(defn checksum [fs]
  (loop [fs fs i 0 sum 0]
    (if (seq fs)
      (let [chunk (first fs)]
        (if-let [gap (:gap chunk)]
          (recur (rest fs)
                 (+ i gap)
                 sum)
          (let [{:keys [id len]} (first fs)]
            (recur (rest fs)
                   (+ i len)
                   (+ sum (apply + (map (partial * id) (range i (+ i len)))))))))
      sum)))

(comment (defrag (expand (parse "12345")))
         (defrag (expand (parse example))))

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
