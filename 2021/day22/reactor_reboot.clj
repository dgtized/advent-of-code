(ns reactor-reboot
  (:require [clojure.string :as str]))

(defn extract-range [s]
  (mapv parse-long (str/split s #"\.\.")))

(defn parse [filename]
  (mapv (fn [line] (let [[value & coords] (rest (re-find #"(on|off) x=(.*),y=(.*),z=(.*)$" line))]
                    [(mapv extract-range coords) (= "on" value)]))
        (str/split-lines (slurp filename))))

(defn inside-50? [x y z]
  (and (<= -50 x 50)
       (<= -50 y 50)
       (<= -50 z 50)))

(defn expand [[[[x0 x1] [y0 y1] [z0 z1]] toggle]]
  (for [x (range (max x0 -50) (min 51 (inc x1)))
        y (range (max y0 -50) (min 51 (inc y1)))
        z (range (max z0 -50) (min 51 (inc z1)))]
    [[x y z] toggle]))

(assert (= 27 (count (expand [[[10 12] [10 12] [10 12]] true]))))

(defn part1 [input]
  (reduce (fn [s cube]
            (reduce
             (fn [s2 [c v]] (if v (assoc s2 c v) (dissoc s2 c))) s (expand cube)))
          {} input))

(assert (= 39 (time (count (part1 (parse "example"))))))
(assert (= 590784 (time (count (part1 (parse "example1"))))))
(assert (= 582644 (time (count (part1 (parse "input"))))))

(defn v- [a b]
  (mapv - a b))

(defn v+ [a b]
  (mapv + a b))

(defn aabb [[[x0 x1] [y0 y1] [z0 z1]]]
  {:p [x0 y0 z0]
   :size (v- [x1 y1 z1] [x0 y0 z0])})

(defn volume [{:keys [size]}]
  (apply * (mapv (fn [x] (Math/abs (inc x))) size)))

(assert (= 27 (volume (aabb [[10 12] [10 12] [10 12]]))))

;; cribbed from thi.ng/geom
(defn aabb-overlap? [{pa :p size-a :size} {pb :p size-b :size}]
  (let [qa (v+ pa size-a)
        qb (v+ pb size-b)]
    (when (and (<= (pa 0) (qb 0)) (<= (pb 0) (qa 0)))
      (when (and (<= (pa 1) (qb 1)) (<= (pb 1) (qa 1)))
        (and (<= (pa 2) (qb 2)) (<= (pb 2) (qa 2)))))))

;; cribbed from thi.ng/geom
(defn aabb-intersection [{pa :p size-a :size} {pb :p size-b :size}]
  (let [qa (v+ pa size-a)
        qb (v+ pb size-b)
        p' (mapv max pa pb)
        q' (mapv min qa qb)
        s' (v- q' p')]
    (when (every? pos? s')
      (aabb (mapv (fn [a b] [a (+ a b)]) p' s')))))

(comment (aabb-intersection (aabb [[0 2] [0 2] [0 2]]) (aabb [[-1 1] [0 2] [0 2]])))

(defn intersecting-boxes [input]
  (->> (for [[coords toggle] input]
         [(aabb coords) toggle])
       (reduce (fn [existing [box toggle]]
                 (let [e' (reduce (fn [acc [cell cell-toggle]]
                                    (if-let [isec (aabb-intersection box cell)]
                                      (conj acc [isec (not cell-toggle)])
                                      acc))
                                  existing existing)]
                   (if toggle
                     (conj e' [box toggle])
                     e')))
               [])))

(defn volumes [boxes]
  (reduce (fn [acc [box toggle]]
            ((if toggle + -) acc (volume box)))
          0 boxes))

(assert (= 39 (volumes (intersecting-boxes (parse "example")))))
;; off by 10?
(assert (= 2758514936282235 (volumes (intersecting-boxes (parse "example2")))))

