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

(defn compile-state [in]
  (let [mods (conj (distinct (mapcat (fn [{:keys [module out]}] (conj out module))
                                     in))
                   "button")]
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
     (conj in {:module "button" :out ["broadcast"]}))))

(defn part1 [in]
  (compile-state in))

(assert (= (part1 (parse example))))
(assert (= (part1 (parse example2))))
(assert (= (part1 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
