(ns aoc.day-11-monkey-in-the-middle
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

#_(aoc/day-input :day 11 :name "example" :content "")
#_(aoc/day-input :day 11)

{::clerk/visibility {:result :hide}}

(def e-mod (* 23 19 13 17))

(def example
  [{:items [79 98]
    :op (fn [x] (* x 19))
    :test (fn [x] (if (= 0 (mod x 23)) 2 3))}
   {:items [54 65 75 74]
    :op (fn [x] (+ x 6))
    :test (fn [x] (if (= 0 (mod x 19)) 2 0))}
   {:items [79 60 97]
    :op (fn [x] (* x x))
    :test (fn [x] (if (= 0 (mod x 13)) 1 3))}
   {:items [74]
    :op (fn [x] (+ x 3))
    :test (fn [x] (if (= 0 (mod x 17)) 0 1))}])

(def i-mod (* 19 7 17 13 11 2 5 3))

(def input
  [{:items [72 97]
    :op (fn [x] (* x 13))
    :test (fn [x] (if (= 0 (mod x 19)) 5 6))}
   {:items [55 70 90 74 95]
    :op (fn [x] (* x x))
    :test (fn [x] (if (= 0 (mod x 7)) 5 0))}
   {:items [74 97 66 57]
    :op (fn [x] (+ x 6))
    :test (fn [x] (if (= 0 (mod x 17)) 1 0))}
   {:items [86 54 53]
    :op (fn [x] (+ x 2))
    :test (fn [x] (if (= 0 (mod x 13)) 1 2))}
   {:items [50 65 78 50 62 99]
    :op (fn [x] (+ x 3))
    :test (fn [x] (if (= 0 (mod x 11)) 3 7))}
   {:items [90]
    :op (fn [x] (+ x 4))
    :test (fn [x] (if (= 0 (mod x 2)) 4 6))}
   {:items [88 92 63 94 96 82 53 53]
    :op (fn [x] (+ x 8))
    :test (fn [x] (if (= 0 (mod x 5)) 4 7))}
   {:items [70 60 71 69 77 70 98]
    :op (fn [x] (* x 7))
    :test (fn [x] (if (= 0 (mod x 3)) 2 3))}])

(defn monkey [input i]
  (let [{:keys [items op] :as monkey} (nth input i)]
    (-> (reduce (fn [input item]
                  (let [worry (int (/ (op item) 3))
                        to ((:test monkey) worry)]
                    (update-in input [to :items] conj worry)))
                input items)
        (assoc-in [i :items] [])
        (assoc-in [i :inspections] (count items)))))

(defn round [input]
  (reduce monkey input (range (count input))))

(defn star1 [input]
  (->> (iterate round input)
       rest
       (take 20)
       (mapv (fn [step] (map :inspections step)))
       (reduce (partial mapv +))
       (sort >)
       (take 2)
       (apply *)))

(defn monkey2 [lcm input i]
  (let [{:keys [items op] :as monkey} (nth input i)]
    (-> (reduce (fn [input item]
                  (let [worry (mod (op item) lcm)
                        to ((:test monkey) worry)]
                    (update-in input [to :items] conj worry)))
                input items)
        (assoc-in [i :items] [])
        (assoc-in [i :inspections] (count items)))))

(defn round2 [lcm input]
  (reduce (partial monkey2 lcm) input (range (count input))))

(defn star2 [input lcm]
  (->> (iterate (partial round2 lcm) input)
       rest
       (take 10000)
       (mapv (fn [step] (map :inspections step)))
       (reduce (partial mapv +))
       (sort >)
       (take 2)
       (apply *)
       ))

{::clerk/visibility {:result :show}}
(star1 example)
(star1 input)
(star2 example e-mod) ;; 2713310158
(star2 input i-mod)
