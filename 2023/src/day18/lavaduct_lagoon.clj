(ns day18.lavaduct-lagoon
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day18/input"))
(def example (slurp "src/day18/example"))

(def dirs {"R" [1 0]
           "L" [-1 0]
           "D" [0 1]
           "U" [0 -1]})

(def directions [[1 0] [-1 0] [0 1] [0 -1]])

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[dir dist color] (str/split line #" ")]
      [(get dirs dir) (parse-long dist) (str/replace color #"[()]" "")])))

(defn v+ [a b] (mapv + a b))
(defn v* [[x y] c] [(* x c) (* y c)])

(defn outline [steps]
  (let [[grid pos]
        (reduce (fn [[grid pos] [dir dist color]]
                  [(reduce (fn [g c] (assoc g (v+ pos (v* dir c)) color))
                           grid (range 0 dist))
                   (v+ pos (v* dir dist))])
                [{} [0 0]]
                steps)]
    (println pos)
    grid))

(defn dims [grid]
  (let [xs (map first (keys grid))
        ys (map second (keys grid))]
    [[(dec (apply min xs)) (inc (apply max xs))]
     [(dec (apply min ys)) (inc (apply max ys))]]))

(defn neighbors [[[x0 x1] [y0 y1]] pos]
  (for [d directions
        :let [[x y :as n] (v+ pos d)]
        :when (and (< x0 x x1) (< y0 y y1))]
    n))

(defn flood-outside [grid start]
  (let [dims (dims grid)]
    (loop [outside #{start}
           queue (conj clojure.lang.PersistentQueue/EMPTY start)
           grid grid]
      (if (empty? queue)
        grid
        (let [pos (peek queue)
              val (get grid pos)]
          (cond (nil? val)
                (let [neighbors (neighbors dims pos)]
                  (recur (set/union outside (set neighbors))
                         (into (pop queue) neighbors)
                         (assoc grid pos "O")))
                :else
                (recur outside
                       (pop queue)
                       grid)))))))

(defn region [grid]
  (let [[x y] (dims grid)]
    (for [j (range 0 y)]
      (apply str (mapv (fn [i] (let [v (get grid [i j])]
                                (cond (= v "O")
                                      v
                                      (some? v)
                                      "#"
                                      :else
                                      ".")))
                       (range 0 x))))))

(defn part1 [in]
  (let [grid (outline in)
        field (flood-outside grid [-1 -1])
        r (region grid)
        f (frequencies (apply str r))]
    (println)
    (spit "src/day18/region" (str/join "\n" r))
    (doseq [l r]
      (println l))
    (+ (get f \#) (get f \.))))

(assert (= (part1 (parse example))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
