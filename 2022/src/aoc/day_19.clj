(ns aoc.day-19
  (:require
   [aoc.utility :as aoc]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

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
  (merge-with - ingrediants blueprint-cost))

(defn time-to-purchase [ingrediants bots blueprint-cost]
  (if (every? (fn [[k _]] (> (get bots k 0) 0)) blueprint-cost)
    (->> blueprint-cost
         (mapv (fn [[k v]] (Math/ceil (/ (- v (get k ingrediants 0)) (get bots k))))))
    [25]))

(defn +ingrediants [bots]
  (fn [i] (merge-with + i bots)))

(def START
  {:bots {:ore 1 :clay 0 :obsidian 0 :geode 0}
   :ingrediants {:ore 0 :clay 0 :obsidian 0 :geode 0}})

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
   (assoc START :path path)
   (range 1 25)))

(comment
  (simulate (second (first example))
            [:clay :clay :clay :obsidian :clay :obsidian :geode :geode]))

(defn score [{:keys [ingrediants path]}]
  (- (get ingrediants :geode) (count path)))

(def paths {:ore [:clay :ore]
            :clay [:ore :clay :obsidian]
            :obsidian [:ore :clay :obsidian :geode]
            :geode [:ore :clay :obsidian :geode]})

(comment
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
                   (filter some? s))))))

(defn expand-paths [path]
  (mapcat
   (fn [insert]
     (map (fn [i] (let [[a b] (split-at i path)]
                   (concat a [insert] b)))
          (range (inc (count path)))))
   [:ore :clay :obsidian :geode]))

(comment
  (mapcat expand-paths (expand-paths [:clay :obsidian :geode])))

(def better-table
  {:ore :clay
   :clay :obsidian
   :obsidian :geode
   :geode :geode})

(defn greedy-bot [ingrediants bots blueprint]
  (let [purchasable (keep (fn [[bot cost]]
                            (when (and (can-purchase? ingrediants cost)
                                       (<= (inc (get bots bot))
                                          (apply max (map #(get % bot 0) (vals blueprint)))))
                              bot))
                          blueprint)]
    (if (seq purchasable)
      (let [best (last purchasable)
            next-best (get better-table best)]
        (cond (= best :geode)
              [:geode]
              ;; (let [time (time-to-purchase ingrediants bots
              ;;                              (get blueprint next-best))
              ;;       time-greedy
              ;;       (let [bots' (update bots best inc)]
              ;;         (time-to-purchase ((+ingrediants bots')
              ;;                            (purchase ingrediants (get blueprint best)))
              ;;                           bots'
              ;;                           (get blueprint next-best)))
              ;;       ]
              ;;   (or (= (apply min time) (apply min time-greedy))))
              ;; (concat purchasable [next-best])
              :else
              (conj purchasable nil)))
      [nil])))

(comment
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
              (second (first example))))

(defn max-bot [blueprint bot]
  (apply max 0 (map #(get % bot 0) (vals blueprint))))

(comment
  (let [blueprint (second (first example))]
    (map (fn [bot] [bot (max-bot blueprint bot)])
         (keys blueprint))))

(defn collected [resource]
  (fn [s] (get-in s [:ingrediants resource])))

(defn score-state [t-remain {:keys [ingrediants bots]}]
  (->> [:ore :clay :obsidian :geode]
       (map-indexed (fn [i bot]
                      (* (Math/pow 10 i)
                         (+ (get ingrediants bot)
                            (* t-remain (get bots bot))))))
       (reduce +)))

(defn purchasable [{:keys [ingrediants bots]} blueprint]
  (conj (keep (fn [[bot cost]]
                (when (and (can-purchase? ingrediants cost)
                           (if (= bot :geode)
                             true
                             (< (get bots bot) (max-bot blueprint bot))))
                  bot))
              blueprint)
        nil))

(comment
  (let [blueprint (second (first example))]
    [(purchasable START blueprint)
     (purchasable (-> START
                      (assoc-in [:ingrediants :ore] 20)
                      (assoc-in [:ingrediants :obsidian] 20))
                  blueprint)]))

(defn simulate-search [blueprint limit]
  (loop [time 0
         states #{START}
         seen #{}]
    (let [max-geodes (apply max 0 (map (collected :geode) states))]
      (if (= time limit)
        max-geodes
        (recur (inc time)
               (set (for [{:keys [bots ingrediants] :as state} states
                          bot (purchasable state blueprint)
                          :let [state' (cond-> state
                                         bot (assoc :ingrediants
                                                    (purchase ingrediants (get blueprint bot)))
                                         bot (update-in [:bots bot] inc)
                                         true (update :ingrediants (+ingrediants bots)))]
                          :when (and (not (seen state'))
                                     (>= ((collected :geode) state') max-geodes))]
                      state'))
               (into seen states))))))

(comment (simulate-search (second (first example)) 24)
         ;; 9
         (simulate-search (second (second example)) 24)
         ;; 12
         )

(defn star1 [file]
  (->> file
       parse
       (pmap (fn [[i blueprint]] (* i (simulate-search blueprint 24))))
       (reduce +)))

(defn star2 [file]
  file)

{::clerk/visibility {:result :show}}
(aoc/answer-table
 [star1 star2]
 (aoc/input-files "day19")
 (fn [{:keys [result]}] result))
