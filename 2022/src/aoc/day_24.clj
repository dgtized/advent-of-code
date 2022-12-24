(ns aoc.day-24
  (:require
   [aoc.utility :as aoc]
   [clojure.set :as set]
   [nextjournal.clerk :as clerk]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 24 :name "example" :content "")
#_(aoc/day-input :day 24)

(defn enumerate [xs] (map vector (range) xs))
(defn v+ [a b] (mapv + a b))

(def dmap {\> [1 0] \v [0 1] \< [-1 0] \^ [0 -1]})
(def inverted-dmap (set/map-invert dmap))

(defn make-blizzard [max-x max-y loc dir]
  (let [[x y] loc]
    (fn [t]
      [(case dir
         [1 0] [(inc (mod (+ (dec x) t) (dec max-x))) y]
         [0 1] [x (inc (mod (+ (dec y) t) (dec max-y)))]
         [-1 0] [(inc (mod (- (dec x) t) (dec max-x))) y]
         [0 -1] [x (inc (mod (- (dec y) t) (dec max-y)))])
       dir])))

(defn create-blizzards [grid max-x max-y]
  (keep (fn [[loc c]]
          (when-let [dir (dmap c)]
            (make-blizzard max-x max-y loc dir)))
        grid))

(defn parse [file]
  (let [lines (aoc/file->lines file)
        grid (into {}
                   (for [[j line] (enumerate lines)
                         [i c] (enumerate line)]
                     [[i j] c]))
        max-y (apply max (map second (keys grid)))]
    {:grid grid
     :max-x (apply max (map first (keys grid)))
     :max-y max-y
     :start (some (fn [[i c]] (when (= c \.) [i 0]))
                  (enumerate (first lines)))
     :end (some (fn [[i c]] (when (= c \.) [i max-y]))
                (enumerate (last lines)))}))

(comment
  (map (make-blizzard 6 6 [3 3] [1 0]) (range 30))
  (map (make-blizzard 6 6 [3 3] [-1 0]) (range 30))
  (map (make-blizzard 6 6 [3 3] [0 1]) (range 30))
  (map (make-blizzard 6 6 [3 3] [0 -1]) (range 30)))

(def example (parse "input/day24.example"))
(def complex (parse "input/day24.complex"))
(def input (parse "input/day24.input"))

(defn simulate [{:keys [grid max-x max-y]}]
  (let [blizzards (create-blizzards grid max-x max-y)
        grid' (into {} (map (fn [[loc cell]] (if (contains? #{\. \#} cell)
                                              [loc cell]
                                              [loc \.]))
                            grid))]
    (fn [t]
      (reduce (fn [g blizzard] (let [[loc dir] (blizzard t)]
                                (update g loc
                                        (fn [cell]
                                          (cond (= cell \.)
                                                (inverted-dmap dir)
                                                (char? cell) 2
                                                :else (inc cell))))))
              grid' blizzards))))

(defn show-grid [g max-x max-y]
  (for [y (range (inc max-y))]
    (apply str (map #(get g [% y]) (range (inc max-x))))))

#_(show-grid ((simulate example) 4) 6 6)

(defn successors [state]
  (let [sim (simulate state)
        cache (atom {})]
    (fn [[loc t]]
      (let [t' (inc t)
            grid (or (get @cache t' nil)
                     (let [g (sim t')]
                       (swap! cache assoc t' g)
                       g))]
        (keep (fn [dir] (let [loc' (v+ loc dir)]
                         (when (= \. (get grid loc' nil))
                           [loc' t'])))
              [[0 0] [1 0] [0 1] [-1 0] [0 -1]])))))

(defn manhattan [[x0 y0] [x1 y1]]
  (+ (Math/abs (- x1 x0))
     (Math/abs (- y1 y0))))

#_((successors example) [[1 0] 0])

(defn search [state start end]
  (->> (aoc/a*-search {:heuristic (fn [[loc _]] (manhattan loc end))}
                      (successors state)
                      start
                      (fn [[loc _]] (= loc end)))
       last
       second))

(defn star1 [file]
  (let [{:keys [start end] :as state} (parse file)]
    (search state [start 0] end)))

(defn star2 [file]
  (let [{:keys [start end] :as state} (parse file)
        t-out (search state [start 0] end)
        t-snacks (search state [end t-out] start)]
    (search state [start t-snacks] end)))

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day24")
 (fn [{:keys [result]}] result))
