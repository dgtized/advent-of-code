(ns day12.christmas-tree-farm
  (:require [clojure.string :as str]))

(def input (slurp "src/day12/input"))
(def example (slurp "src/day12/example"))

(defn rotate-shape [rows]
  (mapv (fn [x] (apply str (reverse x)))
        (apply map vector rows)))

(def corner ["#.." "..." "..."])

(take 4 (iterate rotate-shape corner))

(defn flipx-shape [rows]
  (mapv (fn [r] (apply str (reverse r))) rows))

(take 3 (iterate flipx-shape corner))

(defn flipy-shape [rows]
  (vec (reverse rows)))

(take 3 (iterate flipy-shape corner))

(defn symmetries [shape]
  (->> [identity flipy-shape flipy-shape]
       (mapcat (fn [op] (take 3 (iterate rotate-shape (op shape)))))
       distinct))

(symmetries corner)

(defn parse-shape [s]
  (let [[f & ls] (str/split-lines s)]
    [(parse-long (re-find #"\d+" f))
     (vec ls)]))

(defn parse-region [s]
  (let [[r v] (str/split s #": ")]
    {:size (mapv parse-long (re-seq #"\d+" r))
     :quantities (mapv parse-long (re-seq #"\d+" v))}))

(defn parse [in]
  (let [xs (str/split in #"\n\n")]
    {:shapes (into {} (mapv parse-shape (butlast xs)))
     :regions (mapv parse-region (str/split-lines (last xs)))}))

(defn covers [rows]
  (get (frequencies (apply str rows)) \# 0))

(covers corner)

(defn heuristic [shapes]
  (let [sizes (map covers (vals shapes))]
    (fn [{:keys [size quantities]}]
      (let [area (apply * size)
            min-use (reduce + (map * sizes quantities))
            max-fit (* 9 (reduce + quantities))]
        [area max-fit min-use (<= max-fit area) (< min-use area)]))))

(defn part1 [{:keys [shapes regions]}]
  (map (heuristic shapes) regions))

(assert (= (part1 (parse example))))
(assert (= 469 (count (filter (fn [x] (nth x 3)) (part1 (parse input))))))
