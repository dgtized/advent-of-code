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
  (lazy-seq (when (> (count in) 5)
              (let [group (subs in 0 5)]
                (if (= (nth group 0) 0)
                  [(subs group 1)]
                  (cons (subs group 1) (decode-literal (subs in 5))))))))

(defn decode-packet [in]
  (let [version (bits->int (subs in 0 3))
        type-id (bits->int (subs in 3 6))]
    (cond (= type-id 4)
          [version type-id (bits->int (apply str (decode-literal (subs in 6))))])))

(assert (= [6 4 2021] (decode-packet (bit-string "D2FE28"))))
