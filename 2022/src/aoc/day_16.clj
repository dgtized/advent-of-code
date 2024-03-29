(ns aoc.day-16
  (:require
   [aoc.utility :as aoc]
   [clojure.math.combinatorics :as mc]
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
    (aoc/a*-search successors src dst)))

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

(defn sim-path [input open-path]
  (reduce
   (fn [{:keys [active valve] :as state} t]
     (let [next-valve (first (remove (set (keys active)) open-path))
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

(defn useful-valves [input]
  (filter (fn [x] (> (get-in input [x :flow]) 0)) (keys input)))

#_(count (mc/permutations (useful-valves example)))
(count (useful-valves input))
#_(count (mc/permutations (useful-valves input)))

(defn max-order [input visited]
  (:visited
   (reduce (fn [{:keys [valve visited] :as state} t]
             (let [remaining (remove (set visited) (useful-valves input))]
               (if (seq remaining)
                 (let [next (apply max-key
                                   (fn [x]
                                     (let [flow (get-in input [x :flow])
                                           t-rem (- 30 t)]
                                       (* (- t-rem (count (path input valve x))) flow)))
                                   remaining)]
                   (-> state
                       (assoc :valve next)
                       (update :visited conj next)))
                 state)))
           {:valve (if (empty? visited) "AA" (last visited)) :visited visited}
           (range 30))))

#_(max-order example [])
#_(max-order example ["DD"])

(defn gen-swaps [path]
  (distinct (for [i (range (count path))
                  j (range (count path))
                  :when (not= i j)]
              (assoc (assoc path i (nth path j)) j (nth path i)))))

(defn gen-k-swaps [path k]
  (->> [(vec path)]
       (iterate (fn [paths] (distinct (mapcat gen-swaps paths))))
       (take k)
       last))

(defn path-flow [input path]
  (:total (sim-path input path)))

(defn hillclimb-swaps
  ([input evaluate start]
   (let [cost (evaluate input start)]
     (println "start" start cost)
     (hillclimb-swaps input evaluate cost start {start cost})))
  ([input evaluate best path memo]
   (let [states (pmap (fn [swap] [(if-let [score (get memo swap)]
                                   score (evaluate input swap)) swap])
                      (distinct (concat (gen-k-swaps path 3)
                                        (gen-k-swaps (shuffle path) 2)
                                        (gen-k-swaps (shuffle path) 2)
                                        (gen-k-swaps (shuffle path) 2))))
         [cost swap] (apply max-key first states)]
     (cond (> cost best)
           (do (println (count states) best "->" [cost swap])
               (recur input evaluate cost swap
                      (reduce (fn [m [s p]] (assoc m p s)) memo states)))
           :else
           (do (println [best path])
               [best path])))))

(defn star1 [file]
  (let [input (parse file)]
    (loop [valves (useful-valves input) prefix []]
      (if (empty? valves)
        [prefix (:total (sim-path input prefix))]
        (let [[_ subset] (apply max-key first
                                (for [subset (mc/permuted-combinations valves (min 4 (count valves)))]
                                  (let [trial (concat prefix subset (max-order input (concat prefix subset)))]
                                    [(path-flow input trial) subset])))]
          (recur (remove (set (take 2 subset)) valves) (into prefix (take 2 subset))))))))

(count (mc/permuted-combinations (useful-valves input) 4))

(= (star1 "input/day16.example") [["DD" "BB" "JJ" "HH" "EE" "CC"] 1651])
#_(star1 "input/day16.input")

#_(hillclimb-swaps example path-flow ["DD" "JJ" "BB" "HH" "EE" "CC"])
#_(hillclimb-swaps example path-flow (shuffle (useful-valves example)))
(gen-k-swaps ["DD" "JJ" "BB" "HH" "EE" "CC"] 5)

;; [["GR" "GV" "HX" "JI" "XM" "OH" "BX" "UN" "CQ" "OK" "IR" "GB" "TS" "HF" "LC"]
;;  1645] -- too low

(path-flow input ["GR" "GV" "JI" "HX" "XM" "OH" "BX" "UN" "CQ" "OK" "IR" "GB" "TS" "HF" "LC"])

#_(hillclimb-swaps input path-flow (shuffle ["GR" "HX" "JI" "GV" "XM" "OH" "BX" "UN" "CQ" "OK" "IR" "GB" "TS" "HF" "LC"]))
;; lucky hillclimb start gave:
;; [1673 [OK HF CQ GV GR JI XM OH GB BX IR UN TS LC HX]] [1673 [OK HF CQ GV GR JI XM OH GB BX IR UN LC HX TS]]

(defn sim-move [{:keys [active] :as state} input valve-key open-path]
  (let [valve (valve-key state)
        next-valve (first (remove (set (keys active)) open-path))
        valve-flow (get-in input [valve :flow] 0)]
    (if next-valve
      (if (= next-valve valve)
        (assoc-in state [:active valve] valve-flow)
        (assoc state valve-key (second (path input valve next-valve))))
      state)))

(defn sim-dual-path [input mpath epath]
  (reduce
   (fn [{:keys [active] :as state} t]
     (let [flow (apply + (vals active))]
       (-> state
           (sim-move input :mvalve mpath)
           (sim-move input :evalve epath)
           (assoc :time t :flow flow)
           (update :total + flow))))
   {:time 0 :active {} :flow 0 :total 0 :mvalve "AA" :evalve "AA"}
   (range 1 27)))

#_(sim-dual-path example ["JJ" "BB" "CC"] ["DD" "HH" "EE"])

(defn eval-dual-path [input dpath]
  (->> (range (count dpath))
       (map (fn [i]
              (let [[mpath epath] (split-at i dpath)]
                (:total (sim-dual-path input mpath epath)))))
       (apply max)))

#_(hillclimb-swaps example eval-dual-path (shuffle (useful-valves example)))
#_(hillclimb-swaps input eval-dual-path (shuffle (useful-valves input)))

;; best thus far
;; start [UN HF CQ IR OK BX JI OH HX GV GB LC GR XM TS] 1429
;; 143745 1429 -> [1922 [OK HF CQ GV UN GR JI OH HX IR GB LC BX XM TS]]
;; 143745 1922 -> [2238 [OK HF CQ GV UN GR JI XM OH BX GB LC IR HX TS]]
;; 143745 2238 -> [2278 [OK HF CQ GV HX GR JI XM OH BX GB IR LC TS UN]]

;; smaller iterations resulted in correct answer
;; start [TS UN LC CQ OK HX GR XM JI IR BX OH GV HF GB] 1176
;; 5321 1176 -> [1751 [GV UN LC CQ OK HX GR IR JI XM BX OH TS HF GB]]
;; 5321 1751 -> [2011 [GV HF LC CQ OK HX IR GR JI XM BX OH TS UN GB]]
;; 5321 2011 -> [2186 [OK HF GV CQ LC HX IR GR JI XM BX OH TS UN GB]]
;; 5321 2186 -> [2265 [OK HF CQ GV IR HX LC GR JI XM BX OH TS UN GB]]
;; 5321 2265 -> [2342 [OK HF CQ GV HX IR LC GR JI XM OH BX TS UN GB]]
;; 5321 2342 -> [2343 [OK HF CQ GV HX IR LC GR JI XM OH BX GB TS UN]]
;; [2343 [OK HF CQ GV HX IR LC GR JI XM OH BX GB TS UN]]

#_(hillclimb-swaps input eval-dual-path ["OK" "HF" "CQ" "GV" "HX" "GR" "JI" "XM" "OH" "BX" "GB" "IR" "LC" "TS" "UN"])

(defn star2 [file]
  file)

;; {::clerk/visibility {:result :show}}
;; (aoc/answer-table
;;  [star1 star2]
;;  (aoc/input-files "day16")
;;  (fn [{:keys [result]}] result))

(comment
  (defn search
    [input cost-fn neighborhood-fn]
    (let [evaluate (partial cost-fn input)
          start (useful-valves input)]
      (loop [queue '(start)
             best (evaluate start)
             best-path start
             memo {start best}]
        (if (empty? queue)
          [best path]
          (let [trial (first queue)
                cost (evaluate trial)
                [best best-path] (if (> cost best) [cost trial] [best best-path])]
            (recur (rest queue) best best-path (assoc memo trial cost))))))))
