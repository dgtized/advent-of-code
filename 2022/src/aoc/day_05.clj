(ns aoc.day-05
  {::clerk/visibility {:result :hide}}
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

(defn parse [file]
  (let [[stacks moves]
        (split-with (fn [line] (not= line ""))
                    (file->lines file))]
    [(->> stacks
          butlast
          (map (fn [line] (mapv str (str/replace (str/replace line #"\s\s\s\s" "[_]") #"[\[\]\s+]" ""))))
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
