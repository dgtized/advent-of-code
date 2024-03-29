(ns day25.snowverload
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/day25/input"))
(def example (slurp "src/day25/example"))

(defn parse [in]
  (into {}
        (for [line (str/split-lines in)
              :let [[src & deps] (re-seq #"\w+" line)]]
          [src deps])))

(defn nodes [g]
  (reduce (fn [nodes [node conns]]
            (into (conj nodes node) conns))
          #{} g))

(defn edges [g]
  (reduce (fn [pairs [node conns]]
            (into pairs (map (partial vector node) conns)))
          #{} g))

(defn successors [g]
  (reduce (fn [sg [p q]]
            (-> sg
                (update p (fnil conj []) q)
                (update q (fnil conj []) p)))
          {} (edges g)))

(defn connected-set [successors start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start) seen #{}]
    (if-let [node (peek queue)]
      (recur (into (pop queue) (remove seen (successors node)))
             (conj seen node))
      seen)))

(defn reflexive
  "Ensure graph edges are bidirectional"
  [graph]
  (reduce (fn [g [v w]]
            (update g w (fnil conj []) v))
          graph (edges graph)))

(comment (reflexive {:a [:b] :c [:a]}))

(defn contract [graph v w]
  (let [edges (graph w)]
    (as-> graph G
      (reduce (fn [g edge]
                (if (= v edge)
                  g
                  (update g v conj edge)))
              G edges)
      (reduce (fn [g edge]
                ;; FIXME: why does this stackoverflow sometimes?
                (let [gr (update g edge (partial remove #{w}))]
                  (if (= v edge)
                    gr
                    (update gr edge conj v))))
              G edges)
      (dissoc G w))))

;; https://en.wikipedia.org/wiki/Karger%27s_algorithm
(defn karger-min-cut [graph]
  (loop [graph graph
         groups (zipmap (keys graph) (mapv (fn [x] (set [x])) (keys graph)))]
    (if (> (count graph) 2)
      (let [v (rand-nth (keys graph))
            w (rand-nth (vec (get graph v [])))]
        (recur (contract graph v w)
               (-> groups
                   (update v set/union (get groups w))
                   (dissoc w))))
      {:graph graph
       :groups groups})))

;; TODO:
;; Karger-Stein compares the best case of two recursive karger-stein iterations
;; see https://web.stanford.edu/class/archive/cs/cs161/cs161.1166/lectures/lecture15.pdf

(defn part1 [graph]
  (let [rg (reflexive graph)]
    (loop [trials 20]
      (if-let [v (or (some (fn [{:keys [graph groups]}]
                             (when (= (count (graph (ffirst graph))) 3)
                               (apply * (map count (vals groups)))))
                           (pmap (fn [_] (karger-min-cut rg)) (range 4)))
                     (zero? trials))]
        v
        (recur (dec trials))))))

(assert (= 54 (part1 (parse example))))
;; 1475 nodes, 3312 edges, and between 4 and 9 edges
;; edge distribution {5 362, 4 957, 8 9, 6 119, 9 2, 7 26}
(assert (= 543564 (part1 (parse input))))
