(ns monster-messages
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(defn count-valid [file change-rules]
  (let [[rules messages] (map str/split-lines (str/split (slurp file) #"\n\n"))
        fixed-rules (replace change-rules rules)
        grammar (str "S = 0\n" (str/replace (str/join "\n" fixed-rules) ": " " = "))
        parser (insta/parser grammar)
        valid (remove (fn [msg] (insta/failure? (insta/parse parser msg))) messages)]
    (count valid)))

(def fixes {"8: 42" "8: 42 | 42 8"
            "11: 42 31" "11: 42 31 | 42 11 31"})

(comment
  (= 2 (count-valid "example" {}))
  (= 3 (count-valid "example2" {}))
  (= 147 (count-valid "input" {}))

  (= 12 (count-valid "example2" fixes))
  (= 263 (count-valid "input" fixes)))

