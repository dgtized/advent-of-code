(ns aoc.day-07
  (:require
   [aoc.utility :refer [answer-table file->lines]]
   [nextjournal.clerk :as clerk]
   [clojure.string :as str]))

{::clerk/visibility {:result :hide}}
(defn parse [line]
  (if-let [[_ fname] (re-matches #"^\$ cd (.*)$" line)]
    {:type :cd :name fname}
    (if-let [[_] (re-matches #"^\$ ls$" line)]
      {:type :cmd :name "ls"}
      (if-let [[_ size-or-dir fname] (re-matches #"^(.*) (.*)$" line)]
        (if (= "dir" size-or-dir)
          {:type :dir :name fname :size 0}
          {:type :file :name fname :size (parse-long size-or-dir)})
        line))))

(defn directories [file]
  (let [input (map parse (file->lines file))]
    (->> input
         (reduce (fn [{:keys [path] :as state} {:keys [type name] :as line}]
                   (let [path (if (= type :cd)
                                (cond (= name "..")
                                      (if (= path "/")
                                        "/"
                                        (str/replace path #"/([^/]*)/$" "/"))
                                      (= name "/")
                                      "/"
                                      :else (str path name "/"))
                                path)]
                     (-> state
                         (assoc :path path)
                         (update :input conj (assoc line :path path)))))
                 {:path "/" :input []})
         :input
         (filter #(#{:file :dir} (:type %)))
         (group-by :path)
         )))

#_(directories "input/day07.input")

(defn get-parent [index dir]
  (when-let [[_ parent _] (re-matches #"^(.*/)([^/]+)/$" dir)]
    (if (or (contains? index parent) (= "/" parent))
      parent
      (recur index parent))))

(defn process [file]
  (let [dirs (directories file)
        isized (->> (for [[dir files] dirs]
                      {:dir dir :size (reduce + (map :size files))})
                    (sort-by #(count (:dir %)) >))]
    (->> isized
         (reduce (fn [index {:keys [dir]}]
                   (if-let [parent (get-parent index dir)]
                     (update index parent (fnil + 0) (get index dir))
                     index))
                 (into {} (map (juxt :dir :size)) isized))
         (filter (fn [[f s]] (<= s 100000)))
         )))

#_(process "input/day07.example")
#_(process "input/day07.input")

{::clerk/visibility {:result :show}}
(answer-table
 [(partial process)]
 ["input/day07.example" "input/day07.input"]
 (fn [{:keys [result]}] [(apply + (map second result))
                        result]))
