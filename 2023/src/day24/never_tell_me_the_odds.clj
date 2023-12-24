(ns day24.never-tell-me-the-odds
  (:require [clojure.string :as str]))

(def input (slurp "src/day24/input"))
(def example (slurp "src/day24/example"))

(defn parse [in]
  (for [line (str/split-lines in)]
    (map (fn [p] (mapv parse-long (str/split p #",\s*")))
         (str/split line #"\s*@\s*"))))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(def v+ (partial mapv +))
(def v- (partial mapv -))
(defn v* [a b] (mapv (partial * b) a))
(defn v-dot [u v] (apply +' (map *' u v)))
(defn sqr [x] (* x x))
(defn sqrt [x] (Math/sqrt x))
(defn v-len-sqr [u] (apply + (map sqr u)))
(def v-len (comp sqrt v-len-sqr))
(def v-abs (partial map abs))

(defn v-cross [[a1 a2 a3] [b1 b2 b3]]
  [(- (*' a2 b3) (*' a3 b2))
   (- (*' a3 b1) (*' a1 b3))
   (- (*' a1 b2) (*' a2 b1))])

(defn ray-ray-isec-t1 [[p1 _] [p2 d2] cross]
  (/ (v-dot (v-cross (v- p2 p1) d2) cross)
     (v-len-sqr cross)))

(defn ray-ray-isec-t2 [[p1 d1] [p2 _] cross]
  (/ (v-dot (v-cross (v- p2 p1) d1) cross)
     (v-len-sqr cross)))

(defn isec-ray-ray [r1 r2]
  (let [[p1 d1] r1
        [p2 d2] r2
        cross (v-cross d1 d2)]
    (when-not (every? zero? cross) ;; parallel/coincident
      (let [t1 (float (ray-ray-isec-t1 r1 r2 cross))
            t2 (float (ray-ray-isec-t2 r1 r2 cross))]
        (when (and (>= t1 0) (>= t2 0))
          (let [p1' (mapv float (v+ p1 (v* d1 t1)))
                p2' (mapv float (v+ p2 (v* d2 t2)))]
            {:p1 p1' :t1 t1 :p2 p2 :t2 t2
             :hit (< (v-len (v- p2' p1')) 100)
             :isec (v* (v+ p2' p1') 0.5)}))))))

(defn in-test-area? [l0 l1 [x y _]]
  (and (<= l0 x l1) (<= l0 y l1)))

(defn part1 [l0 l1 rays]
  (for [[r1 r2] (all-pairs rays)
        :let [hit (isec-ray-ray r1 r2)]
        :when (and hit (:hit hit)
                   (in-test-area? l0 l1 (:isec hit))
                   (in-test-area? l0 l1 (:p1 hit))
                   (in-test-area? l0 l1 (:p2 hit))
                   )]
    [[r1 r2] hit]))

(assert (= 2 (count (part1 7 27 (parse example)))))
;; (assert (= (part1 200000000000000 400000000000000 (parse input))))

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
