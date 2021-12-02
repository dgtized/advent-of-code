(ns dive
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn load-commands [filename]
  (for [line (str/split-lines (slurp filename))
        :let [[c n] (str/split line #"\s+")]]
    [c (edn/read-string n)]))

(def example (load-commands "example"))
(def commands (load-commands "input"))

(defn first-star [commands]
  (reduce (fn [state [cmd arg]]
            (case cmd
              "forward" (update state :horizontal + arg)
              "down" (update state :depth + arg)
              "up" (update state :depth - arg)))
          {:horizontal 0 :depth 0} commands))

(defn second-star [commands]
  (reduce (fn [{:keys [aim] :as state} [cmd arg]]
            (case cmd
              "forward" (-> state
                            (update :horizontal + arg)
                            (update :depth + (* aim arg)))
              "down" (update state :aim + arg)
              "up" (update state :aim - arg)))
          {:aim 0 :horizontal 0 :depth 0} commands))

(defn answer [{:keys [horizontal depth] :as state}]
  (assoc state :answer (* horizontal depth)))

(assert (= {:horizontal 15 :depth 10 :answer 150}
           (answer (first-star example))))

(assert (= {:aim 10 :horizontal 15 :depth 60 :answer 900}
           (answer (second-star example))))

(println "First Star:" (answer (first-star commands)))
(println "Second Star:" (answer (second-star commands)))


