(ns day20.pulse-propagation
  (:require [clojure.string :as str]))

(def input (slurp "src/day20/input"))
(def example (slurp "src/day20/example"))
(def example2 (slurp "src/day20/example2"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[module dests] (str/split line #" -> ")
          type (subs module 0 1)
          out (str/split dests #",\s*")]
      (if (contains? #{"%" "&"} type)
        {:module (subs module 1)
         :type (if (= type "%")  :flip-flop :conjunction)
         :out out}
        {:module module
         :out out}))))

(zipmap (range 3) (repeatedly (fn [] 1)))

(defn compile-state [in]
  (let [mods (conj (distinct (mapcat (fn [{:keys [module out]}] (conj out module))
                                     in))
                   "button")
        state
        (reduce
         (fn [s {:keys [module type out]}]
           (reduce (fn [s t]
                     (update-in s [t :in] (fnil conj #{}) module))
                   (update s module merge
                           {:module module
                            :out out}
                           (when type {:type type}))
                   out))
         (into {} (for [mod mods]
                    [mod {}]))
         (conj in {:module "button" :out ["broadcaster"]}))]
    (reduce-kv
     (fn [s m {:keys [type in]}]
       (case type
         :flip-flop
         (assoc-in s [m :state] false)
         :conjunction
         (assoc-in s [m :state] (zipmap in (repeatedly (fn [] false))))
         s))
     state
     state)))

(defn propagate [to dest type state signal]
  (case type
    :flip-flop
    (when-not signal
      [to dest state])
    :conjunction
    [to dest (not (every? true? (vals state)))]
    [to dest signal]))

(defn press [modules]
  (loop [pulses []
         queue (conj clojure.lang.PersistentQueue/EMPTY ["button" "broadcaster" false])
         modules modules]
    (if (empty? queue)
      [modules pulses]
      (let [[from to signal :as pulse] (peek queue)
            {:keys [out type]} (get modules to)
            modules' (case type
                       :flip-flop
                       (if signal
                         modules
                         (update-in modules [to :state] not))
                       :conjunction
                       (assoc-in modules [to :state from] signal)
                       modules)
            state (get-in modules' [to :state])]
        (recur (conj pulses pulse)
               (into (pop queue)
                     (keep (fn [dest] (propagate to dest type state signal))
                           out))
               modules')))))

(defn n-presses [in]
  (nth (iterate (fn [[modules low high]]
                  (let [[modules' pulses] (press modules)]
                    [modules'
                     (+ low (count (remove last pulses)))
                     (+ high (count (filter last pulses)))]))
                [(compile-state in) 0 0]) 1000))

(defn part1 [in]
  (apply * (rest (n-presses in))))

(assert (= 32000000 (part1 (parse example))))
(assert (= 11687500 (part1 (parse example2))))
(assert (= 841763884 (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
