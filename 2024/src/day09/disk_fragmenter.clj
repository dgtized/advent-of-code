(ns day09.disk-fragmenter
  (:require [clojure.string :as str]))

(def input (slurp "src/day09/input"))
(def example (slurp "src/day09/example"))

(defn parse [in]
  (map (comp parse-long str) (str/trimr in)))

(defn expand [layout]
  (for [[idx [len gap]] (map-indexed vector (partition-all 2 2 layout))]
    {:idx idx :len len :gap (if gap gap 0)}))

(defn defrag [chunks]
  (loop [chunks (vec chunks) fs []]
    (if (seq chunks)
      (let [{:keys [idx len gap]} (first chunks)]
        (if (or (zero? gap) (empty? (rest chunks)))
          (recur (rest chunks)
                 (conj fs {:idx idx :len len}))
          (let [{lidx :idx llen :len} (last chunks)]
            (recur (concat
                    [{:idx lidx
                      :len (min llen gap)
                      :gap (if (< gap llen) 0 (- gap llen))}]
                    (rest (drop-last 1 chunks))
                    (when (< gap llen)
                      [{:idx lidx
                        :len (- llen gap)
                        :gap 0}]))
                   (conj fs {:idx idx :len len})))))
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
;; slow 6146.933911 msecs
#_(time (assert (= 6607511583593 (part1 (parse input)))))

(comment (expand (parse input)))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
