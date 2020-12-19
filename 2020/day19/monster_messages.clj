(ns monster-messages
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(defn count-valid [file]
  (let [[rules messages] (map str/split-lines (str/split (slurp file) #"\n\n"))
        grammar (into ["S = 0"] (map #(str/replace % ": " " = ") rules))
        parser (insta/parser (str/join "\n" grammar))
        valid (remove (fn [msg] (insta/failure? (insta/parse parser msg))) messages)]
    (count valid)))

(comment
  (count-valid "example")
  (count-valid "input"))

