(ns aoc.day-05-supply-stacks
  {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

(defn parse [file]
  (let [[stacks moves]
        (->> file
             file->lines
             (split-with (fn [line] (not= line ""))))]
    [(->> stacks
          butlast
          (map (fn [line]
                 (as-> line _
                   (str/replace _ #"\s\s\s\s" "[_]")
                   (str/replace _ #"[\[\]\s+]" "")
                   (mapv str _))))
          (apply map vector)
          (mapv (comp reverse (partial remove #{"_"}))))
     (map (fn [line] (map parse-long (re-seq #"\d+" line))) (rest moves))]))

(defn move [op stacks n from to]
  (let [src (nth stacks (dec from))
        dst (nth stacks (dec to))]
    (-> stacks
        (assoc (dec from) (drop-last n src))
        (assoc (dec to) (concat dst (op (take-last n src)))))))

(defn process [op]
  (fn [file]
    (let [[stacks moves] (parse file)]
      (reduce (fn [s m] (apply (partial move op) s m))
              stacks moves))))

{::clerk/visibility {:result :show}}
(answer-table
 [(process reverse) (process identity)]
 ["input/day05.example" "input/day05.input"]
 (fn [{:keys [result]}]
   [(apply str (map last result))]))

(defn rows->cols [state]
  (reverse (apply map vector
                  (let [height (apply max (map count state))]
                    (for [col state]
                      (map #(nth col % "") (range height)))))))

#_(rows->cols ['("Z" "N") '("M" "C" "D") '("P")])

(defn state-move? [x]
  (and (map? x) (contains? x :state) (contains? x :move)))

(clerk/with-viewers
  (clerk/add-viewers
   [{:pred vector?
     :transform-fn (fn [input]
                     (clerk/col (get input :nextjournal/value)))}
    {:pred state-move?
     :transform-fn (fn [input]
                     (let [{:keys [state move]} (get input :nextjournal/value)
                           [n src dst] move]
                       (clerk/row (if (= move nil)
                                    (clerk/code "initial stack state")
                                    (clerk/code (str "move " n " from " src " to " dst)))
                                  (clerk/table (rows->cols state)))))}])
  (let [[stacks moves] (parse "input/day05.input")
        state-move (map (fn [s m] {:state s :move m})
                        (reductions (fn [s m] (apply (partial move reverse) s m))
                                    stacks moves)
                        (conj moves nil))]
    (vec (take 10 state-move))))
