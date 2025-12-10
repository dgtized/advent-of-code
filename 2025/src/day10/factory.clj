(ns day10.factory
  (:require [clojure.string :as str]
            [aoc.graph :as graph]))

(def input (slurp "src/day10/input"))
(def example (slurp "src/day10/example"))

(Integer/toBinaryString (bit-xor 2r0000 2r0110))
(Integer/toBinaryString (bit-xor 2r0010 2r0110))

(Integer/toBinaryString (bit-xor 2r000001 2r011001))

(defn state->num [s]
  (Integer/parseInt
   (-> s
       (str/replace #"\." "0")
       (str/replace #"\#" "1"))
   2))

(defn button->num [xs]
  (reduce (fn [s bits] (bit-or s (bit-shift-left 1 bits))) 0 xs))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[_ goal xs] (re-find #"\[(.*)\] (.*)" line)
          bs (map second (re-seq #"[({]((\d+,?)+)[)}]" xs))
          buttons (mapv (fn [button] (mapv parse-long (re-seq #"\d+" button))) (butlast bs))]
      {:goal (state->num (apply str (reverse goal)))
       :raw-goal goal
       :buttons (mapv button->num buttons)
       :bits buttons
       :extra (mapv parse-long (re-seq #"\d+" (last bs)))})))

(defn find-path [{:keys [goal buttons]}]
  (graph/a*-search
   {:successors (fn [value] (mapv (fn [b] (bit-xor b value)) buttons))
    :sources buttons
    :goal? (fn [value] (= value goal))}))

(defn part1 [in]
  (mapv find-path in))

(println)
(find-path (nth (parse example) 0))
(bit-xor (bit-xor 0 2r101) 2r11)
(find-path (nth (parse example) 1))
(Integer/toBinaryString (bit-xor (bit-xor (bit-xor 0 2r10001) 2r00111) 2r11110))

(assert (= 7 (reduce + (map count (part1 (parse example))))))
(assert (= 479 (reduce + (map count (part1 (parse input))))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
