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

(defn dir-sizes [file]
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
         )))

#_(dir-sizes "input/day07.example")
#_(dir-sizes "input/day07.input")

(defn part1 [file]
  (let [r (->> (dir-sizes file)
               (filter (fn [[_ s]] (<= s 100000))))]
    [(apply + (map second r)) r]))

(defn part2 [file]
  (let [sizes (dir-sizes file)
        root (get sizes "/")
        target (- 70000000 root)
        threshold (- 30000000 target)]
    [(some (fn [d] (when (> (second d) threshold)
                    (second d)))
           (sort-by second < sizes))
     threshold]))

{::clerk/visibility {:result :show}}
(answer-table
 [part1 part2]
 ["input/day07.example" "input/day07.input"]
 (fn [{:keys [result]}] result))
