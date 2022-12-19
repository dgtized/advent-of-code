(ns aoc.day-19
  (:require
   [aoc.utility :as aoc]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}

#_(aoc/day-input :day 19 :name "example" :content "")
#_(aoc/day-input :day 19)

(defn parse [file]
  (for [blueprint (rest (str/split (slurp file) #"Blueprint "))
        :let [[_ n] (re-find #"(\d+):" blueprint)]]
    [(parse-long n)
     (into {} (for [[_ kind costs] (re-seq #"Each (\w+) robot costs ([^\.]+)\." blueprint)]
                [(keyword kind)
                 (into {} (map (fn [[_ x ingredient]] [(keyword ingredient) (parse-long x)])
                               (re-seq #"(\d+) (\w+)" costs)))]))]))

(def example (parse "input/day19.example"))
(def input (parse "input/day19.input"))

(defn can-purchase? [ingrediants blueprint-cost]
  (every? (fn [[k v]] (>= (get ingrediants k 0) v))
          blueprint-cost))

(defn purchase [ingrediants blueprint-cost]
  (reduce-kv (fn [i k v] (update i k - v)) ingrediants blueprint-cost))

(defn simulate [blueprint path]
  (reductions
   (fn [{:keys [bots ingrediants path] :as state} _]
     (let [bot (first path)
           blueprint-cost (get blueprint bot)]
       (-> (if (and bot (can-purchase? ingrediants blueprint-cost))
             (-> state
                 (update-in [:bots bot] inc)
                 (assoc :ingrediants (purchase ingrediants blueprint-cost))
                 (update :path rest))
             state)
           (update :ingrediants
                   (fn [i]
                     (reduce-kv (fn [i kind n] (update i kind + n)) i bots))))))
   {:bots {:ore 1 :clay 0 :obsidian 0 :geode 0}
    :ingrediants {:ore 0 :clay 0 :obsidian 0 :geode 0}
    :path path}
   (range 1 25)))

(simulate (second (first example))
          [:clay :clay :clay :obsidian :clay :obsidian :geode :geode])

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day19")
 (fn [{:keys [result]}] result))
