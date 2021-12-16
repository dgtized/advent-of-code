(ns packet-decoder
  (:require [clojure.string :as str]))

(defn parse [filename]
  (str/trim (slurp filename)))

(defn bit-string [input]
  (->> input
       (map (fn [c] (str/replace
                    (format "%4s"
                            (-> c
                                str
                                (Integer/parseInt 16)
                                Integer/toBinaryString))
                    #"\s" "0")))
       (apply str)))

;; (bit-string (parse "input"))
(assert (= "110100101111111000101000" (bit-string "D2FE28")))
(assert (= "00111000000000000110111101000101001010010001001000000000" (bit-string "38006F45291200")))

(defn bits->int [input]
  (Integer/parseInt input 2))

(assert (= 2 (bits->int "010")))

(defn decode-literal [in]
  (lazy-seq (when (>= (count in) 5)
              (let [group (subs in 0 5)]
                (if (= (nth group 0) \0)
                  [(subs group 1)]
                  (cons (subs group 1) (decode-literal (subs in 5))))))))

(declare decode-packets)
(defn decode-packet [in base]
  (let [version (bits->int (subs in base (+ base 3)))
        type-id (bits->int (subs in (+ base 3) (+ base 6)))
        control-id (bits->int (subs in (+ base 6) (+ base 7)))]
    (cond (= type-id 4)
          (let [groups (decode-literal (subs in 6))
                size (+ 6 (* (count groups) 5))]
            {:type :literal :size size
             :version version :type-id type-id
             :value (bits->int (apply str groups))})
          (= control-id 0)
          (let [sub-packet-length (bits->int (subs in (+ base 7) (+ base 22)))
                children (decode-packets (subs in (+ base 22) (+ base 22 sub-packet-length)))
                size (+ 22 sub-packet-length)]
            {:type :sub-bit :size size
             :version version :type-id type-id
             :length sub-packet-length
             :children (vec children)})
          (= control-id 1)
          (let [n-sub-packets (bits->int (subs in (+ base 7) (+ base 18)))
                children (decode-packets (subs in (+ base 18)) n-sub-packets)
                size (+ 18 (apply + (map :size children)))]
            {:type :sub-n :size size
             :version version :type-id type-id
             :n n-sub-packets
             :children (vec children)}))))

(defn decode-packets
  ([in] (decode-packets in -1))
  ([in limit]
   (loop [base 0 packets []]
     (if (or (<= (- (count in) base) 10)
             (= (count packets) limit))
       packets
       (let [packet (decode-packet in base)]
         (recur (+ base (:size packet)) (conj packets packet)))))))

(assert (= [{:type :literal, :size 21, :version 6, :type-id 4, :value 2021}]
           (decode-packets (bit-string "D2FE28"))))
(assert (= [{:type :sub-bit,
             :size 49,
             :version 1,
             :type-id 6,
             :length 27,
             :children
             [{:type :literal, :size 11, :version 6, :type-id 4, :value 10}
              {:type :literal, :size 11, :version 2, :type-id 4, :value 10}]}]
           (decode-packets (bit-string "38006F45291200"))))
(assert (= [{:type :sub-n,
             :size 51,
             :version 7,
             :type-id 3,
             :n 3,
             :children
             [{:type :literal, :size 11, :version 2, :type-id 4, :value 1}
              {:type :literal, :size 11, :version 4, :type-id 4, :value 1}
              {:type :literal, :size 11, :version 1, :type-id 4, :value 1}]}]
           (decode-packets (bit-string "EE00D40C823060"))))

(assert (= [{:type :sub-n, :size 69,
             :version 4,
             :type-id 2,
             :n 1,
             :children
             [{:type :sub-n, :size 51,
               :version 1,
               :type-id 2,
               :n 1,
               :children
               [{:type :sub-bit, :size 33,
                 :version 5,
                 :type-id 2,
                 :length 11,
                 :children [{:type :literal, :size 11, :version 6, :type-id 4, :value 15}]}]}]}]
           (decode-packets (bit-string "8A004A801A8002F478"))))

(defn part1 [in]
  (if (map? in)
    (apply + (map :version (tree-seq :children :children in)))
    (apply + (map part1 in))))

(assert (= 16 (part1 (decode-packets (bit-string "8A004A801A8002F478")))))
(assert (= 12 (part1 (decode-packets (bit-string "620080001611562C8802118E34")))))
(assert (= 23 (part1 (decode-packets (bit-string "C0015000016115A2E0802F182340")))))
(assert (= 31 (part1 (decode-packets (bit-string "A0016C880162017C3686B18A3D4780")))))

(decode-packet (bit-string (parse "input")))
;; (assert (= 31 (part1 (decode-packet (bit-string (parse "input"))))))
