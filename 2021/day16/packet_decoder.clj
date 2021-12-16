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

(defn decode-packet [in offset]
  (println)
  (println in)
  (loop [base offset packets []]
    (if (< (- (count in) base) 12)
      packets
      (let [version (bits->int (subs in base (+ base 3)))
            type-id (bits->int (subs in (+ base 3) (+ base 6)))
            control-id (bits->int (subs in (+ base 6) (+ base 7)))]
        (println [base version type-id (subs in base)])
        (cond (= type-id 4)
              (let [groups (decode-literal (subs in 6))
                    size (+ 6 (* (count groups) 5))]
                (recur
                 (+ base size)
                 (conj packets [[:literal size] version type-id (bits->int (apply str groups))])))
              (= control-id 0)
              (let [sub-packet-length (bits->int (subs in (+ base 7) (+ base 22)))
                    children (decode-packet (subs in (+ base 22) (+ base 22 sub-packet-length)) 0)
                    size (apply + (map #(get-in % [0 1]) children))]
                (recur (+ base 22 sub-packet-length)
                       (conj packets [[:sub-bit size] version type-id children])))
              (= control-id 1)
              (let [n-sub-packets (bits->int (subs in (+ base 7) (+ base 18)))
                    children (take n-sub-packets (decode-packet (subs in (+ base 18)) 0))
                    size (apply + (map #(get-in % [0 1]) children))]
                (recur (+ base 18 size)
                       (conj packets [[:sub-n size] version type-id children]))))))))

(assert (= [[[:literal 21] 6 4 2021]] (decode-packet (bit-string "D2FE28") 0)))
(assert (= [[[:sub-bit 22] 1 6 [[[:literal 11] 6 4 10] [[:literal 11] 2 4 10]]]]
           (decode-packet (bit-string "38006F45291200") 0)))
(assert (= [[[:sub-n 33] 7 3
             [[[:literal 11] 2 4 1] [[:literal 11] 4 4 1] [[:literal 11] 1 4 1]]]]
           (decode-packet (bit-string "EE00D40C823060") 0)))
