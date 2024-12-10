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
(assert (= 6607511583593 (time (part1 (parse input)))))

(comment (expand (parse input)))

(defn find-file [file-id chunks]
  (some (fn [[idx chunk]]
          (when (= (:id chunk) file-id) [chunk idx]))
        (map-indexed vector chunks)))

(defn without-file [file chunks]
  (replace {file {:gap (:len file)}} chunks))

(defn gap-index [size chunks]
  (loop [idx 0]
    (when (< idx (count chunks))
      (let [gap (get (nth chunks idx) :gap 0)]
        (if (>= gap size)
          idx
          (recur (inc idx)))))))

(defn collapse [gap chunks]
  (let [[gaps removed] (split-with :gap chunks)]
    (if (and (zero? gap) (empty? gaps))
      removed
      (into [{:gap (+ gap (apply + (map :gap gaps)))}]
            removed))))

(comment (collapse 1 [{:gap 1} {:gap 2} {:id 5 :len 2}]))

(defn defrag-fit [chunks]
  (let [max-id (apply max (map :id (filter :id chunks)))]
    (loop [file-id max-id chunks chunks]
      (if (neg? file-id)
        chunks
        (let [[file fidx] (find-file file-id chunks)
              idx (gap-index (:len file) chunks)]
          (if (and idx (< idx fidx))
            (let [gap (nth chunks idx)
                  [before after] (split-at idx chunks)]
              (recur (dec file-id)
                     (vec (concat before
                                  [file]
                                  (collapse (max (- (:gap gap) (:len file)) 0)
                                            (without-file file (rest after)))))))
            (recur (dec file-id)
                   chunks)))))))

(comment (defrag-fit (expand (parse example))))

(defn part2 [in]
  (checksum (defrag-fit (expand in))))

(assert (= 2858 (part2 (parse example))))
;; slow 27635.252687ms
(assert (= 6636608781232 (time (part2 (parse input)))))
