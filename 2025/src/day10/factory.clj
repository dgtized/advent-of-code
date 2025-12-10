(ns day10.factory
  (:require
   [aoc.graph :as graph]
   [clojure.math :as math]
   [clojure.string :as str]))

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
       :jolts (mapv parse-long (re-seq #"\d+" (last bs)))})))

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
;; (assert (= 479 (reduce + (map count (part1 (parse input))))))

(defn bit-add [sum bits]
  (reduce (fn [s b] (update s b (fnil inc 0))) sum bits))

(comment (bit-add [0 0 0] [0 2]))

(defn pad [xs len]
  (let [c (count xs)]
    (if (< c len)
      (into (vec (repeat (- len c) 0)) xs)
      xs)))

(pad [0] 3)
(pad [1 0] 3)

(defn bit-dist [a b]
  (let [len (max (count a) (count b))]
    (math/sqrt (reduce + (mapv (fn [l r] (math/pow (- l r) 2))
                               (pad a len) (pad b len))))))

(bit-dist [1 0 1] [0 0 1])

(defn add [a b]
  (mapv + a b))

(defn find-counter [{:keys [jolts bits]}]
  (let [moves (mapv (fn [b] (bit-add (pad [] (count jolts)) b)) bits)]
    (time (graph/a*-search
           {:successors (fn [curr] (mapv (fn [m] (add curr m)) moves))
            :sources [(pad [] (count jolts))]
            :cost (fn [curr last] (bit-dist curr last))
            :heuristic (fn [curr] (bit-dist jolts curr))
            :goal? (fn [curr] (= jolts curr))}))))

(find-counter (nth (parse example) 1))

(defn part2 [in]
  (mapv (comp dec count find-counter) in))

(assert (= 33 (reduce + (part2 (parse example)))))
;; (assert (= (reduce + (part2 (parse input)))))

;; (println (reduce + (part2 (parse input))))
