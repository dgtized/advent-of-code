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

(defn rotation-matrices []
  (let [rows (->> [[1 0 0] [0 1 0] [0 0 1]]
                  (mapcat (juxt identity #(v* % -1))))]
    (for [r1 rows
          r2 rows
          r3 rows
          :let [m (vec (concat r1 r2 r3))]
          :when (= 1 (determinant m))]
      m)))

(defn mat* [[a b c d e f g h i] [x y z]]
  [(+ (* x a) (* y b) (* z c))
   (+ (* x d) (* y e) (* z f))
   (+ (* x g) (* y h) (* z i))])

(let [orientations (vec (rotation-matrices))]
  (defn orientation [basis]
    (nth orientations (mod basis 24))))

(defn oriented [basis coord]
  (mat* (orientation basis) coord))

(assert (mapv oriented (range 24) (repeat [8 0 7])))

;; scanner1 - scanner0 = (beacon0 - scanner0) + (beacon1 - scanner1)
(defn scanner-coord [beacons0 beacons1]
  (some (fn [basis]
          (let [pairs (for [b0 beacons0
                            b1 beacons1]
                        (v- b0 (oriented basis b1)))
                overlap (filter #(>= (second %) 12) (frequencies pairs))]
            (when (seq overlap)
              (for [[coord n] overlap]
                {:basis basis :n n :coord coord}))))
        (range 24)))

(defn scanner-pairs [input]
  (for [[a b] (all-pairs (keys input))
        :let [coords (scanner-coord (get input a) (get input b))]
        :when coords]
    [a b coords]))

(def example (parse "example"))
(def puzzle (parse "input"))

(scanner-pairs (parse "example"))

(comment (for [basis (range 24)]
           [basis (v+ [-618 -824 -621] (oriented basis [686 422 578]))]))

(frequencies (for [basis (range 24)]
               (oriented basis [68 -1246 -43])))

(assert (= [{:basis 4 :n 12 :coord [68 -1246 -43]}]
           (scanner-coord (get example 0) (get example 1))))

(defn translate [beacons basis coord]
  (mapv #(v+ (oriented basis %) coord) beacons))

(assert (some #{[-618 -824 -621]} (translate (get example 1) 4 [68 -1246 -43])))

(defn scanner-overlap [beacons scanners]
  (flatten
   (for [scan-id (keys scanners)
         :let [coords (scanner-coord beacons (get scanners scan-id))]
         :when coords]
     (for [c coords]
       (assoc c :scan-id scan-id)))))

(assert (= [{:basis 4, :n 12, :coord [68 -1246 -43], :scan-id 1}]
           (scanner-overlap (set (get example 0)) (dissoc example 0))))

(defn solve [input]
  (loop [beacons (set (get input 0))
         found {}
         scanners (dissoc input 0)]
    (if (empty? scanners)
      {:beacons (count beacons) :found found}
      (if-let [scanning (seq (scanner-overlap beacons scanners))]
        (let [{:keys [scan-id basis coord]} (apply max-key :n scanning)]
          (recur (into beacons (translate (get scanners scan-id) basis coord))
                 (assoc found scan-id [basis coord])
                 (dissoc scanners scan-id)))
        {:beacons beacons :found found :scanners-remaining (keys scanners)}))))

(assert (= {:beacons 79,
            :found
            {1 [4 [68 -1246 -43]],
             4 [15 [-20 -1133 1061]],
             3 [4 [-92 -2380 -20]],
             2 [6 [1105 -1205 1229]]}})
        (time (solve (parse "example"))))

(assert (= {:beacons 496,
            :found
            {7 [5 [3785 48 1211]],
             20 [16 [2566 -1130 -2367]],
             27 [8 [1382 -30 -20]],
             1 [17 [1212 -1236 3556]],
             24 [22 [124 -1141 2453]],
             39 [19 [1253 14 2421]],
             4 [2 [8571 -45 -1296]],
             15 [22 [3624 61 -1278]],
             21 [12 [2477 -47 2]],
             31 [6 [1258 -1243 1158]],
             32 [0 [150 60 -2414]],
             33 [16 [2450 92 1130]],
             13 [7 [2490 -5 -1182]],
             22 [23 [1254 -1265 2471]],
             36 [16 [172 2 -1300]],
             29 [0 [7324 -63 -1168]],
             6 [15 [4832 80 -84]],
             28 [11 [1267 42 1112]],
             25 [16 [1280 -2409 2479]],
             34 [5 [1367 -2453 -57]],
             17 [6 [2565 1122 -1218]],
             3 [20 [1252 -1184 4724]],
             12 [21 [3752 1215 -1257]],
             2 [9 [3607 -2449 -97]],
             23 [18 [1276 54 3592]],
             35 [7 [1273 1108 78]],
             19 [13 [1280 -1226 -40]],
             11 [1 [1205 1142 -1150]],
             9 [4 [2530 2402 -1217]],
             5 [10 [2480 -3 -2458]],
             14 [11 [3687 -1240 -1176]],
             26 [15 [6132 -1177 -1173]],
             16 [8 [6041 14 -1191]],
             38 [8 [1370 -2400 -1165]],
             30 [2 [4847 -15 -1282]],
             10 [3 [2537 1246 -2376]],
             18 [14 [1321 2335 49]],
             37 [22 [2557 -1262 2305]],
             8 [19 [2468 -2482 55]]}}
           (time (solve (parse "input")))))

(defn manhattan [v1 v2]
  (apply + (mapv (fn [v] (Math/abs v)) (v- v1 v2))))

(assert (manhattan [1105 -1205 1229] [-92 -2380 -20]))

(->> {7 [5 [3785 48 1211]],
      20 [16 [2566 -1130 -2367]],
      27 [8 [1382 -30 -20]],
      1 [17 [1212 -1236 3556]],
      24 [22 [124 -1141 2453]],
      39 [19 [1253 14 2421]],
      4 [2 [8571 -45 -1296]],
      15 [22 [3624 61 -1278]],
      21 [12 [2477 -47 2]],
      31 [6 [1258 -1243 1158]],
      32 [0 [150 60 -2414]],
      33 [16 [2450 92 1130]],
      13 [7 [2490 -5 -1182]],
      22 [23 [1254 -1265 2471]],
      36 [16 [172 2 -1300]],
      29 [0 [7324 -63 -1168]],
      6 [15 [4832 80 -84]],
      28 [11 [1267 42 1112]],
      25 [16 [1280 -2409 2479]],
      34 [5 [1367 -2453 -57]],
      17 [6 [2565 1122 -1218]],
      3 [20 [1252 -1184 4724]],
      12 [21 [3752 1215 -1257]],
      2 [9 [3607 -2449 -97]],
      23 [18 [1276 54 3592]],
      35 [7 [1273 1108 78]],
      19 [13 [1280 -1226 -40]],
      11 [1 [1205 1142 -1150]],
      9 [4 [2530 2402 -1217]],
      5 [10 [2480 -3 -2458]],
      14 [11 [3687 -1240 -1176]],
      26 [15 [6132 -1177 -1173]],
      16 [8 [6041 14 -1191]],
      38 [8 [1370 -2400 -1165]],
      30 [2 [4847 -15 -1282]],
      10 [3 [2537 1246 -2376]],
      18 [14 [1321 2335 49]],
      37 [22 [2557 -1262 2305]],
      8 [19 [2468 -2482 55]]}
     vals
     (mapv second)
     all-pairs
     (mapv #(apply manhattan %))
     (apply max))
