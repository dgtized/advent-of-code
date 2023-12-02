(ns day02.day02
  (:require [clojure.string :as str]))

(def input (slurp "src/day02/input"))
(def example (slurp "src/day02/example"))

(defn pair [s]
  (let [[_ n kind] (re-matches #"(\d+) (.*)" s)]
    [kind (parse-long n)]))

(defn possible? [{:keys [sets]}]
  (let [lookup {"red" 12
                "green" 13
                "blue" 14}]
    (every? (fn [[kind value]] (<= value (lookup kind))) sets)))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[_ id contents] (re-matches #"^Game (\d+): (.*)" line)
          sets (str/split contents #";\s*")]
      {:id (parse-long id)
       :sets (mapcat (fn [s] (map pair (str/split s #", "))) sets)})))

(comment (parse example))
(assert (= 2283 (apply + (map :id (filter possible? (parse input))))))
;; (parse input)
