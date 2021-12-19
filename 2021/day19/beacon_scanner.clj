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


(defn vec3 [x y z]
  [x y z])

(defn v+ [a b]
  (mapv + a b))

(defn v- [a b]
  (mapv - a b))

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

(assert (->> (for [rotation [x-rotation y-rotation z-rotation]
                   t (range 0 (* 2 Math/PI) (* 0.5 Math/PI))
                   facing [-1 1]]
               (mat* (rotation facing t) [8 0 7]))
             count
             (= 24)))