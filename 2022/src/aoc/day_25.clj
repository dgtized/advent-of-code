(ns aoc.day-25
  (:require
   [clojure.set :as set]
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 25 :name "example" :content "")
#_(aoc/day-input :day 25)

(defn parse [file]
  (aoc/file->lines file))

(def example (parse "input/day25.example"))
(def input (parse "input/day25.input"))

(def digit->number {\0 0 \1 1 \2 2 \= -2 \- -1})
(def number->digit (set/map-invert digit->number))

(defn decode [s]
  (->> s
       (mapv digit->number)
       (map-indexed (fn [p digit]
                      (let [place (long (Math/pow 5 (- (dec (count s)) p)))]
                        (* place digit))))
       (reduce +)))

(defn carry [digits]
  (if (empty? digits)
    '(1)
      (let [d (inc (first digits))]
        (cond (< d 3)
              (cons d (rest digits))
              (= d 3)
              (cons -2 (carry (rest digits)))))))

(defn encode-slow [n]
  (apply str (map number->digit (reverse (first (drop n (iterate carry '(0))))))))

(map (fn [n snafu] [n snafu (encode-slow n)]) (range) (take 30 (iterate carry '(0))))
(encode-slow 2022)
(encode-slow 4890)

;; 0 (carry '(0))
;; 1 (carry '(1))
;; 2 (carry '(2))
;; 3 (carry '(-2 1))
;; 4 (carry '(-1 1))
;; 5 (carry '(0 1))
;; 6 (carry '(1 1))
;; 7 (carry '(2 1))
;; 8 (carry '(-2 2))
;; 9 (carry '(-1 2))
;; 10 (carry '(0 2))
;; 11 (carry '(1 2))
;; 12 (carry '(2 2))
;; 13 (carry '(-2 -2 1))
;; 14 (carry '(-1 -2 1))
;; 15 (carry '(0 -2 1))
;; 16 (carry '(1 -2 1))
;; 17 (carry '(2 -2 1))

(defn encode-b5 [n]
  (loop [n n digits []]
    (let [q (quot n 5)
          r (mod n 5)]
      (if (= n 0)
        digits
        (recur q (conj digits r))))))

;; (encode-b5 8)
;; (encode-b5 9)
;; (encode-b5 10)

(defn encode [n]
  ;; (println)
  (loop [n n digits '()]
    (let [[q r] [(quot n 5) (mod n 5)]]
      ;; (println [n q r] digits (carry digits))
      (cond (= n 0)
            [digits (apply str (map number->digit digits))]
            (<= r 2)
            (recur q (cons r digits))
            (= r 3)
            (recur q (cons -2 (carry digits)))
            (= r 4)
            (recur q (cons -1 (carry digits)))
            :else
            [n q r]))))

;; (map (juxt identity encode) (range 30))

;; (encode 8)
;; (encode 9)
;; (encode 10)
;; (encode 11)
;; (encode 2022)
;; (encode 12345)

;; (map (juxt identity (comp encode decode)) example)

(defn star1 [file]
  (let [n (apply + (map decode (parse file)))]
    [n (encode n)]))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day25")
 (fn [{:keys [result]}] result))
