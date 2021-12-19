(ns beacon-scanner
  (:require [clojure.string :as str]))

(defn parse [filename]
  (into {} (for [group (rest (str/split (slurp filename) #"--- scanner "))]
             [(parse-long (last (re-find #"(\d+) ---\n" group)))
              (->> group
                   str/split-lines
                   rest
                   (mapv (comp (partial mapv parse-long)
                               #(re-seq #"\-?\d+" %))))])))

(defn all-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (all-pairs s))))

(defn vec3 [x y z]
  [x y z])

(defn v+ [a b]
  (mapv + a b))

(defn v- [a b]
  (mapv - a b))

(defn v* [v n]
  (mapv (partial * n) v))

;; rotation: n,s,e,w
;; facing direction: +x,-x, +y,-y, +z, -z
;; yields 24 alignments

(defn determinant [[a b c d e f g h i]]
  (+ (* a (- (* e i) (* f h)))
     (* (- b) (- (* d i) (* f g)))
     (* c (- (* d h) (* e g)))))

(defn x-rotation [facing t]
  [facing 0 0
   0 (int (Math/cos t)) (int (- (Math/sin t)))
   0 (int (Math/sin t)) (int (Math/cos t))])

(defn y-rotation [facing t]
  [(int (Math/cos t)) 0 (int (Math/sin t))
   0 facing 0
   (int (- (Math/sin t))) 0 (int (Math/cos t))])

(defn z-rotation [facing t]
  [(int (Math/cos t)) (int (- (Math/sin t))) 0
   (int (Math/sin t)) 0 (int (Math/cos t))
   0 0 facing])

(defn mat* [[a b c d e f g h i] [x y z]]
  [(+ (* x a) (* y b) (* z c))
   (+ (* x d) (* y e) (* z f))
   (+ (* x g) (* y h) (* z i))])

(let [orientations
      (into [] (for [rotation [x-rotation y-rotation z-rotation]
                     theta (range 0 (* 2 Math/PI) (* 0.5 Math/PI))
                     facing [1 -1]]
                 (partial mat* (rotation facing theta))))]
  (defn orientation [basis]
    (nth orientations (mod basis 24))))

(defn oriented [basis coord]
  ((orientation basis) coord))

(assert (mapv oriented (range 24) (repeat [8 0 7])))

;; scanner1 - scanner0 = (beacon0 - scanner0) + (beacon1 - scanner1)
(defn scanner-coord [beacons0 beacons1]
  (for [basis0 (range 24)
        basis1 (range 24)]
    (let [pairs (for [b0 beacons0
                      b1 beacons1]
                  (v+ (oriented basis0 b0) (oriented basis1 b1)))
          overlap (filter #(>= (second %) 4) (frequencies pairs))]
      [(mapv describe-orientation [basis0 basis1]) overlap])))

(defn basis-overlap [beacons0 beacons1]
  (filter (fn [[basis overlap]] (when (seq overlap) [basis overlap]))
          (scanner-coord beacons0 beacons1)))

(defn scanner-pairs [input]
  (let [size (count input)]
    (for [[a b] (all-pairs (range size))]
      [a b (basis-overlap (get input a) (get input b))])))

(scanner-pairs (parse "example"))


(comment (for [basis (range 24)]
           [basis (v+ [-618 -824 -621] (oriented basis [686 422 578]))]))
