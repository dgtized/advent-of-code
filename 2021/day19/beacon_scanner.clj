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
      (into [] (for [theta [0 90 180 270]
                     rotation [:x :y :z]
                     facing [1 -1]]
                 [rotation facing theta]))]
  (defn describe-orientation [basis]
    (nth orientations (mod basis 24)))

  (defn orientation [basis]
    (let [[rotation facing theta] (nth orientations (mod basis 24))]
      ((get {:x x-rotation :y y-rotation :z z-rotation} rotation)
       facing
       (/ (* theta Math/PI) 180)))))

(defn oriented [basis coord]
  (mat* (orientation basis) coord))

(mapv (comp determinant orientation) (range 24))

(assert (mapv oriented (range 24) (repeat [8 0 7])))

;; scanner1 - scanner0 = (beacon0 - scanner0) + (beacon1 - scanner1)
(defn scanner-coord [beacons0 beacons1]
  (mapcat (fn [basis]
            (let [pairs (for [b0 beacons0
                              b1 beacons1]
                          (v+ b0 (oriented basis b1)))
                  overlap (filter #(>= (second %) 6) (frequencies pairs))]
              (when (seq overlap)
                (for [[coord n] overlap]
                  {:basis basis :n n :coord coord}))))
          (range 24)))

(defn scanner-pairs [input]
  (for [[a b] (all-pairs (keys input))
        :let [coords (scanner-coord (get input a) (get input b))]
        :when (seq coords)]
    [a b coords]))

(def example (parse "example"))
(def puzzle (parse "input"))

(scanner-pairs (parse "example"))

(comment (for [basis (range 24)]
           [basis (v+ [-618 -824 -621] (oriented basis [686 422 578]))]))

(frequencies (for [basis (range 24)]
               (oriented basis [68 -1246 -43])))

(assert (= [{:basis 3, :n 12, :coord [68 -1246 -43]}]
           (scanner-coord (get example 0) (get example 1))))

(defn translate [beacons basis coord]
  (mapv #(v- coord (oriented basis %)) beacons))

(assert (some #{[-618 -824 -621]} (translate (get example 1) 3 [68 -1246 -43])))

(defn scanner-overlap [beacons scanners]
  (for [scan-id (keys scanners)
        :let [coords (scanner-coord beacons (get scanners scan-id))]
        :when (seq coords)]
    (for [[basis n coord] coords]
      {:scan-id scan-id :basis basis :n n :coord coord})))

(scanner-overlap (set (get example 0)) (dissoc example 0))

(defn solve [input]
  (loop [beacons (set (get input 0))
         scanners (dissoc input 0)]
    (if (empty? scanners)
      beacons
      (let [{:keys [scan-id basis _ coord]}
            (apply max-key :n (scanner-overlap beacons scanners))]
        (recur (into beacons (translate (get scanners scan-id) basis coord))
               (dissoc scanners scan-id))))))

(solve example)
