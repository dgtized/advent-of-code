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
       (map-indexed
        (fn [i digit]
          (let [place (long (Math/pow 5 (- (dec (count s)) i)))]
            (* place (digit->number digit)))))
       (reduce +)))

(defn carry [digits]
  (if (empty? digits)
    '(1)
      (let [d (inc (first digits))]
        (if (< d 3)
          (cons d (rest digits))
          (cons -2 (carry (rest digits)))))))

(defn encode-slow [n]
  (apply str (map number->digit (reverse (first (drop n (iterate carry '(0))))))))

#_(encode-slow 2022)
#_(encode-slow 4890)
#_(map (fn [n snafu] [n snafu (encode-slow n)]) (range) (take 30 (iterate carry '(0))))

(defn encode-b5 [n]
  (when-not (zero? n)
    (conj (encode-b5 (quot n 5)) (mod n 5))))

;; (encode-b5 8)  ;; 1 3 => 2 -2  => 2=
;; (encode-b5 9)  ;; 1 4 => 2 -1  => 2-
;; (encode-b5 10) ;; 2 0 => 2 0   => 20
;; (encode-b5 2022) ;; 3 1 0 4 2 => 1 -2 1 1 -1 2 => 1=11-2
;; (encode-b5 12345) ;; 3 4 3 3 4 0 => 1 -1 0 -1 -1 -1 0 =? 1-0---0

;; 3 => 1 -2
;; 4 => 1 -1 -1
;; 3 => 1 -1 0 -1
;; 3 => 1 -1 0 -1 -1
;; 4 => 1 -1 0 -1 -1 -1
;; 0 => 1 -1 0 -1 -1 -1 0

(defn convert [xs]
  (->> xs
       (reduce (fn [digits d]
                 (cond (= d 3)
                       (cons -2 (carry digits))
                       (= d 4)
                       (cons -1 (carry digits))
                       :else
                       (cons d digits)))
               '())
       reverse))

(= [1 -2 1 1 -1 2]
   (convert [3 1 0 4 2])
   (convert (reverse (encode-b5 2022))))

(defn encode-a [n]
  (->> n
       encode-b5
       reverse
       convert
       (map number->digit)
       (apply str)))

(defn encode-b [n]
  (loop [n n digits '()]
    (let [q (quot n 5)
          r (mod n 5)]
      (cond (= n 0)
            (apply str (map number->digit digits))
            (= r 3)
            (recur q (cons -2 (carry digits)))
            (= r 4)
            (recur q (cons -1 (carry digits)))
            :else
            (recur q (cons r digits))))))

#_(map (juxt identity encode-slow encode-a encode-b) (concat (range 30) [2022 12345]))
#_(map (juxt identity decode (comp encode-a decode) (comp encode-b decode)) example)

(defn star1 [file]
  (let [n (apply + (map decode (parse file)))]
    [n (encode-a n)]))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1]
 (aoc/input-files "day25")
 (fn [{:keys [result]}] result))
