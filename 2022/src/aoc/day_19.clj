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

(defn time-to-purchase [ingrediants bots blueprint-cost]
  (if (every? (fn [[k _]] (> (get bots k 0) 0)) blueprint-cost)
    (->> blueprint-cost
         (mapv (fn [[k v]] (quot (- v (get k ingrediants 0)) (get bots k)))))
    [25]))

(defn +ingrediants [bots]
  (fn [i] (reduce-kv (fn [i kind n] (update i kind + n)) i bots)))

(defn simulate [blueprint path]
  (reduce
   (fn [{:keys [bots ingrediants path] :as state} _]
     (let [bot (first path)
           blueprint-cost (get blueprint bot)]
       (-> (if (and bot (can-purchase? ingrediants blueprint-cost))
             (-> state
                 (update-in [:bots bot] inc)
                 (assoc :ingrediants (purchase ingrediants blueprint-cost))
                 (update :path rest))
             state)
           (update :ingrediants (+ingrediants bots)))))
   {:bots {:ore 1 :clay 0 :obsidian 0 :geode 0}
    :ingrediants {:ore 0 :clay 0 :obsidian 0 :geode 0}
    :path path}
   (range 1 25)))

(simulate (second (second example))
          [:clay :clay :clay :obsidian :clay :obsidian :geode :geode])


(defn score [{:keys [ingrediants path]}]
  (- (get ingrediants :geode) (count path)))

(def paths {:ore [:clay :ore]
            :clay [:ore :clay :obsidian]
            :obsidian [:ore :clay :obsidian :geode]
            :geode [:ore :clay :obsidian :geode]})

(let [blueprint (second (second example))]
  (apply max-key
         (fn [[_ run]] (score run))
         (pmap (fn [path] [path (simulate blueprint path)])
               (for [a [:ore nil]
                     b [:ore nil]
                     c [:ore :clay]
                     d [:ore :clay :obsidian nil]
                     e [:ore :clay :obsidian nil]
                     f [:clay :obsidian :geode nil]
                     g [:clay :obsidian :geode nil]
                     h [:clay :obsidian :geode nil]
                     i [:clay :obsidian :geode nil]
                     j [:clay :obsidian :geode nil]
                     k [:clay :obsidian :geode nil]
                     l [:clay :obsidian :geode nil]
                     m [:clay :obsidian :geode]
                     n [:obsidian :geode]
                     :let [s [a b c d e f g h i j k l m n :geode :geode :geode :geode :geode :geode]]]
                 (filter some? s)))))

(defn search [blueprint]
  (loop [queue []]))

(defn greedy-bot [ingrediants bots blueprint]
  (let [purchasable (keep (fn [[bot cost]]
                            (when (can-purchase? ingrediants cost)
                              bot))
                          blueprint)
        future (map (fn [[bot cost]] [bot (time-to-purchase ingrediants bots cost)]) blueprint)]
    [purchasable
     future
     (mapv (fn [[bot _]]
             (let [bots' (update bots bot inc)]
               (map (fn [[bot cost]]
                      [bot (time-to-purchase ((+ingrediants bots') ingrediants) bots' cost)]
                      )
                    blueprint)))
           future)
     (first (remove (set purchasable) (keys blueprint)))]))

(greedy-bot {:ore 0 :clay 0 :obsidian 0 :geode 0}
            {:ore 1 :clay 0 :obsidian 0 :geode 0}
            (second (first example)))

;; clay

(greedy-bot {:ore 2 :clay 0 :obsidian 0 :geode 0}
            {:ore 1 :clay 0 :obsidian 0 :geode 0}
            (second (first example)))

;; clay

(greedy-bot {:ore 2 :clay 3 :obsidian 0 :geode 0}
            {:ore 1 :clay 2 :obsidian 0 :geode 0}
            (second (first example)))

(defn simulate-greedy [blueprint]
  (reductions
   (fn [{:keys [bots ingrediants] :as state} _]
     (let [bot (greedy-bot ingrediants bots blueprint)
           blueprint-cost (get blueprint bot nil)]
       (-> (if (and bot (can-purchase? ingrediants blueprint-cost))
             (-> state
                 (update-in [:bots bot] inc)
                 (assoc :ingrediants (purchase ingrediants blueprint-cost)))
             state)
           (update :ingrediants (+ingrediants bots)))))
   {:bots {:ore 1 :clay 0 :obsidian 0 :geode 0}
    :ingrediants {:ore 0 :clay 0 :obsidian 0 :geode 0}}
   (range 1 25)))

(simulate-greedy (second (first example)))

(defn star1 [file]
  file)

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day19")
 (fn [{:keys [result]}] result))
