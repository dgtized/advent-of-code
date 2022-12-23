(ns aoc.day-23
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.set :as set]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 23 :name "example" :content "")
#_(aoc/day-input :day 23)

(defn parse [file]
  (into {} (for [[j line] (map-indexed vector (reverse (aoc/file->lines file)))
                 [i el] (map-indexed vector line)
                 :when (= el \#)]
             [[i j] el])))

(defn v+ [a b] (mapv + a b))

(def neighbors
  (for [i (range -1 2)
        j (range -1 2)
        :when (not= 0 i j)]
    [i j]))

(def simple (parse "input/day23.simple"))
(def example (parse "input/day23.example"))
(def input (parse "input/day23.input"))

(defn check-empty [dir dirs]
  (fn [neighbors]
    (when (empty? (set/intersection dirs neighbors))
      dir)))

(def checks [(check-empty [0 1] #{[0 1] [1 1] [-1 1]})
             (check-empty [0 -1] #{[0 -1] [1 -1] [-1 -1]})
             (check-empty [-1 0] #{[-1 0] [-1 1] [-1 -1]})
             (check-empty [1 0] #{[1 0] [1 1] [1 -1]})])

(defn neighborhood [grid loc]
  (for [dir neighbors
        :let [pos (v+ loc dir)
              neighbor (get grid pos \.)]
        :when (= neighbor \#)]
    dir))

(defn step [[grid checks _]]
  (let [active-elves
        (keep (fn [[loc v]]
                (when (= v \#)
                  (when-let [neighbors (seq (neighborhood grid loc))]
                    [loc (set neighbors)])))
              grid)
        proposals
        (->> (for [[elf neighbors] active-elves
                   :let [proposed (when-let [dir (some (fn [check] (check neighbors)) checks)]
                                    (v+ elf dir))]
                   :when proposed]
               [elf proposed])
             (group-by second)
             (remove (fn [[_ moves]] (> (count moves) 1)))
             (mapcat second))
        grid' (reduce (fn [g [elf move]]
                        (assoc (dissoc g elf) move \#))
                      grid
                      proposals)]
    ;; (if (< (count grid') (count grid))
    ;;   (println proposals))
    [grid'
     (let [[a b c d] checks]
       [b c d a])
     (empty? proposals)]))

(defn bounds [grid]
  [(apply min (map first (keys grid)))
   (apply max (map first (keys grid)))
   (apply min (map second (keys grid)))
   (apply max (map second (keys grid)))])

(defn show-grid [[grid _]]
  (let [[x-min x-max y-min y-max] (bounds grid)]
    (into []
          (reverse
           (for [j (range (dec y-min) (inc y-max))]
             (apply str (map #(get grid [% j] \.) (range (dec x-min) (inc x-max)))))))))

(defn count-empty [grid]
  (let [[x-min x-max y-min y-max] (bounds grid)]
    (apply + (for [j (range y-min (inc y-max))
                   i (range x-min (inc x-max))
                   :when (not (contains? grid [i j]))]
               1))))

(defn run-steps [grid n]
  (let [grid' (->> [grid checks false]
                   (iterate step)
                   (drop n)
                   first
                   first)]
    ;; (println "")
    ;; (doseq [line (show-grid [grid' nil])]
    ;;   (println line))
    (count-empty grid')))

;; (show-grid (last (take 4 (iterate step [simple checks false]))))
;; (step [example checks false])

;; (run-steps example 10)

(defn star1 [file]
  (run-steps (parse file) 10))

(defn star2 [file]
  (let [grid (parse file)]
    (reduce
     (fn [[g c] i]
       ;; (println (count g'))
       (let [[g' c' done] (step [g c false])]
         (if done
           (reduced (inc i))
           [g' c'])))
     [grid checks]
     (range))))

;; (time (star2 "input/day23.simple"))
;; (time (star2 "input/day23.example"))
;; (time (star2 "input/day23.input"))

{::clerk/visibility {:result :show}}
#_(aoc/answer-table
   [star1 star2]
   (aoc/input-files "day23")
   (fn [{:keys [result]}] result))
