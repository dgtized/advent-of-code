(ns day22.sand-slabs
  (:require [clojure.string :as str]))

(def input (slurp "src/day22/input"))
(def example (slurp "src/day22/example"))

(defn coord [in]
  (mapv parse-long (str/split in #",")))

(defn parse [in]
  (for [line (str/split-lines in)]
    (let [[a b] (str/split line #"~")]
      (map coord [a b]))))

(defn v+ [a b] (mapv + a b))
(defn v- [a b] (mapv - a b))

(defn collide? [a b]
  (let [[[ax0 ay0 az0] [ax1 ay1 az1]] a
        [[bx0 by0 bz0] [bx1 by1 bz1]] b]
    (and (or (<= ax0 bx0 ax1) (<= ax0 bx1 ax1))
         (or (<= ay0 by0 ay1) (<= ay0 by1 ay1))
         (or (<= az0 bz0 az1) (<= az0 bz1 az1)))))

(defn fall-step [box]
  (let [[a b] box]
    [(update a 2 dec) (update b 2 dec)]))

(defn supports? [a b]
  (collide? a (fall-step b)))

(defn supported? [boxes box]
  (or
   (let [[a b] box]
     (or (= (last a) 1) (= (last b) 1)))
   (some (fn [b] (supports? b box)) boxes)))

(comment (fall-step [[1 1 2] [1 1 3]]))

(defn sim-step [boxes box]
  (let [[a b] box]
    (if (and (> (last a) 1)
             (> (last b) 1)
             (not-any? (fn [b] (supports? b box)) (remove #{box} boxes)))
      (fall-step box)
      box)))

(defn simulate-fall [boxes]
  (->> boxes
       (iterate (fn [xs] (mapv (partial sim-step xs) xs)))
       (drop-while (fn [xs] (not-every? (partial supported? xs) xs)))
       first))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn support-graph [boxes]
  (reduce (fn [g [a b]]
            (-> (if (and (not= a b) (supports? a b))
                  (update g a (fnil conj #{}) b)
                  g)))
          {}
          (all-pairs boxes)))

(defn supporting-graph [graph]
  (reduce (fn [g [a supporting]]
            (reduce (fn [g' b]
                      (update g' b (fnil conj #{}) a))
                    g supporting))
          {}
          graph))

(defn disintegrate? [boxes box]
  (let [supporting (take 2 (filter (fn [b] (supports? box b))
                                   (remove #{box} boxes)))]
    (or (> (count supporting) 1))
    ))

(defn part1 [in]
  (let [steady (simulate-fall in)
        support (support-graph steady)
        supporting (supporting-graph support)]
    [support supporting]))

(assert (= (part1 (parse example))))
;; (assert (= (part1 (parse input))))

(defn part2 [in]
  in)

;; (assert (= (part2 (parse example))))
;; (assert (= (part2 (parse input))))
