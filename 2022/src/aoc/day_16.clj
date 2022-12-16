(ns aoc.day-16
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 16 :name "example" :content "")
#_(aoc/day-input :day 16)

(defn parse [file]
  (into {}
        (for [line (aoc/file->lines file)]
          (let [[valve & tunnels] (re-seq #"[A-Z]{2}" line)]
            [valve {:flow (parse-long (re-find #"\d+" line)) :tunnels tunnels}]))))

(def example (parse "input/day16.example"))
(def input (parse "input/day16.input"))

(defn path [input src dst]
  (let [successors (fn [x] (get-in input [x :tunnels]))]
    (aoc/a*-search successors (fn [_ _] 1) (fn [_] 1) src dst)))


#_(path example "AA" "JJ")

(defn paths [input]
  (let [time-cost (fn [x] (count (path input "AA" x)))
        flow-cost (fn [x] (get-in input [x :flow]))
        ranked-nodes (sort-by time-cost < (keys input))]
    (map (juxt identity time-cost flow-cost (fn [x] (* (- 30 (time-cost x)) (flow-cost x)))) ranked-nodes)))

#_(sort-by #(nth % 3) (filter #(> (nth % 3) 0) (paths example)))
#_(sort-by #(nth % 3) (filter #(> (nth % 3) 0) (paths input)))

(defn paths-cost [input valve t]
  (->> (dissoc input valve)
       keys
       (map (juxt identity
                  (fn [x]
                    (let [flow (get-in input [x :flow])
                          t-rem (- 30 t)]
                      (* (- t-rem (count (path input valve x))) flow)))))
       (sort-by second >)))

#_(paths-cost example "AA" 0)

(defn find-next [input valve t]
  (->> (dissoc input valve)
       keys
       (apply max-key
              (fn [x]
                (let [flow (get-in input [x :flow])
                      t-rem (- 30 t)]
                  (* (- t-rem (count (path input valve x))) flow))))))

(defn sim-path [input open-path]
  (reduce
   (fn [{:keys [active valve] :as state} t]
     (let [;; next-valve (find-next (apply dissoc input (keys active)) valve t)
           next-valve (first (remove (set (keys active)) open-path))
           valve-flow (get-in input [valve :flow] 0)
           flow (apply + (vals active))
           state' (if next-valve
                    (if (= next-valve valve)
                      (assoc-in state [:active valve] valve-flow)
                      (-> state
                          (assoc :valve (second (path input valve next-valve)))
                          (assoc :dest next-valve)))
                    state)]
       (-> state'
           (update :time inc)
           (assoc :flow flow)
           (update :total + flow))))
   {:time 0 :active {} :flow 0 :total 0 :valve "AA"}
   (range 30)))

#_(sim-path example ["DD" "BB" "JJ" "HH" "EE" "CC"])

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn useful-valves [input]
  (filter (fn [x] (> (get-in input [x :flow]) 0)) (keys input)))

#_(count (permutations (useful-valves example)))
(count (useful-valves input))
#_(count (permutations (useful-valves input)))

(defn star1 [file]
  (let [input (parse file)
        best (last (sort-by (fn [path] (:total (sim-path input path))) (permutations (useful-valves input))))]
    [path (sim-path input best)]))

(star1 "input/day16.example")

(defn star2 [file]
  file)

;; {::clerk/visibility {:result :show}}
;; (aoc/answer-table
;;  [star1 star2]
;;  (aoc/input-files "day16")
;;  (fn [{:keys [result]}] result))
