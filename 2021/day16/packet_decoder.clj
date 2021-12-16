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
  (Long/parseLong input 2))

(assert (= 1 (bits->int "001")))
(assert (= 2 (bits->int "010")))
(assert (= 3 (bits->int "011")))
(assert (= 7 (bits->int "111")))
(assert (= 48781326955 (bits->int "101101011011100101111111101001101011")))

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
        control-id (bits->int (subs in (+ base 6) (+ base 7)))
        packet {:version version :type-id type-id}]
    (cond (= type-id 4)
          (let [groups (decode-literal (subs in (+ base 6)))
                size (+ 6 (* (count groups) 5))]
            (assoc packet
                   :type :literal
                   :size size
                   :value (bits->int (apply str groups))))
          (= control-id 0)
          (let [sub-packet-length (bits->int (subs in (+ base 7) (+ base 22)))
                children (decode-packets (subs in (+ base 22) (+ base 22 sub-packet-length)))
                size (+ 22 (apply + (map :size children)))]
            (assoc packet
                   :type :operator
                   :size size
                   :length sub-packet-length
                   :children (vec children)))
          (= control-id 1)
          (let [n-sub-packets (bits->int (subs in (+ base 7) (+ base 18)))
                children (decode-packets (subs in (+ base 18)) n-sub-packets)
                size (+ 18 (apply + (map :size children)))]
            (assoc packet
                   :type :operator
                   :size size
                   :n n-sub-packets
                   :children (vec children))))))

(defn decode-packets
  ([in] (decode-packets in -1))
  ([in limit]
   (loop [base 0 packets []]
     (if (or (<= (- (count in) base) 10)
             (= (count packets) limit))
       packets
       (let [packet (decode-packet in base)]
         (recur (+ base (:size packet)) (conj packets packet)))))))

(defn decode [in]
  (decode-packet in 0))

(assert (= {:type :literal, :size 21, :version 6, :type-id 4, :value 2021}
           (decode (bit-string "D2FE28"))))
(assert (= {:type :operator,
            :size 49,
            :version 1,
            :type-id 6,
            :length 27,
            :children
            [{:type :literal, :size 11, :version 6, :type-id 4, :value 10}
             {:type :literal, :size 16, :version 2, :type-id 4, :value 20}]}
           (decode (bit-string "38006F45291200"))))
(assert (= {:type :operator,
            :size 51,
            :version 7,
            :type-id 3,
            :n 3,
            :children
            [{:type :literal, :size 11, :version 2, :type-id 4, :value 1}
             {:type :literal, :size 11, :version 4, :type-id 4, :value 2}
             {:type :literal, :size 11, :version 1, :type-id 4, :value 3}]}
           (decode (bit-string "EE00D40C823060"))))

(assert (= {:type :operator, :size 69,
            :version 4,
            :type-id 2,
            :n 1,
            :children
            [{:type :operator, :size 51,
              :version 1,
              :type-id 2,
              :n 1,
              :children
              [{:type :operator, :size 33,
                :version 5,
                :type-id 2,
                :length 11,
                :children [{:type :literal, :size 11, :version 6, :type-id 4, :value 15}]}]}]}
           (decode (bit-string "8A004A801A8002F478"))))

(defn part1 [in]
  (apply + (map :version (tree-seq :children :children in))))

(assert (= 16 (part1 (decode (bit-string "8A004A801A8002F478")))))
(assert (= 12 (part1 (decode (bit-string "620080001611562C8802118E34")))))
(assert (= 23 (part1 (decode (bit-string "C0015000016115A2E0802F182340")))))
(assert (= 31 (part1 (decode (bit-string "A0016C880162017C3686B18A3D4780")))))

(assert (= 877 (part1 (decode (bit-string (parse "input"))))))
;; (assert (= 31 (part1 (decode-packet (bit-string (parse "input"))))))

(defn evaluate [{:keys [type-id children] :as packet}]
  (case type-id
    0 (apply + (map evaluate children))
    1 (apply * (map evaluate children))
    2 (apply min (map evaluate children))
    3 (apply max (map evaluate children))
    4 (:value packet)
    5 (let [[b a] (map evaluate children)]
        (if (< a b) 1 0))
    6 (let [[b a] (map evaluate children)]
        (if (> a b) 1 0))
    7 (let [[a b] (map evaluate children)]
        (if (= a b) 1 0))))

(assert (= 3 (evaluate (decode (bit-string "C200B40A82")))))
(assert (= 54 (evaluate (decode (bit-string "04005AC33890")))))
(assert (= 7 (evaluate (decode (bit-string "880086C3E88112")))))
(assert (= 9 (evaluate (decode (bit-string "CE00C43D881120")))))
(assert (= 1 (evaluate (decode (bit-string "D8005AC2A8F0")))))
(assert (= 0 (evaluate (decode (bit-string "F600BC2D8F")))))
(assert (= 0 (evaluate (decode (bit-string "9C005AC2F8F0")))))
(assert (= 1 (evaluate (decode (bit-string "9C0141080250320F1802104A08")))))

(assert (= 194435634456 (evaluate (decode (bit-string (parse "input"))))))
