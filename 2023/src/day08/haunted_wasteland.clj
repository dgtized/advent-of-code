(ns day08.haunted-wasteland
  (:require [clojure.string :as str]))

(def input (slurp "src/day08/input"))
(def example (slurp "src/day08/example"))
(def example2 (slurp "src/day08/example2"))
(def example3 (slurp "src/day08/example3"))

(defn parse [in]
  (let [lines (str/split-lines in)]
    {:dirs (vec (first lines))
     :maps
     (into {}
           (for [line (drop 2 lines)]
             (let [[a b c] (re-seq #"\w+" line)]
               [a {\L b \R c}])))}))

(defn path [{:keys [dirs maps]}]
  (loop [pos "AAA" path [] dirs (cycle (seq dirs))]
    (if (= pos "ZZZ")
      path
      (recur (get-in maps [pos (first dirs)])
             (conj path pos)
             (rest dirs)))))

(defn part1 [in]
  (count (path in)))

(assert (= 2 (part1 (parse example))))
(assert (= 6 (part1 (parse example2))))
(assert (= 11309 (part1 (parse input))))

(defn part2 [{:keys [dirs maps]}]
  (let [init (filter #(str/ends-with? % "A") (keys maps))]
    (loop [positions init
           loops (vec (repeat (count init) []))
           steps 0]
      (cond (every? (fn [pos] (str/ends-with? pos "Z")) positions)
            steps
            (every? (fn [l] (> (count l) 1)) loops)
            loops
            :else
            (let [dir (nth dirs (mod steps (count dirs)))]
              (recur (mapv (fn [pos] (get-in maps [pos dir])) positions)
                     (reduce (fn [l [i pos]]
                               (if (str/ends-with? pos "Z")
                                 (update l i conj steps)
                                 l))
                             loops (map-indexed vector positions))
                     (inc steps)))))))

;; (filter #(str/ends-with? % "A") (keys (:maps (parse example3))))

;; borrowed from https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(assert (= 6 (part2 (parse example3))))
(assert (= 13740108158591 (reduce lcm (map first (part2 (parse input))))))
