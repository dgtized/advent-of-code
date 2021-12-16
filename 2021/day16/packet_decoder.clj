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
