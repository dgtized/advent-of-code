(ns monster-messages
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(defn fix [rules change-rules]
  (if (empty? change-rules)
    rules
    (let [change (first change-rules)
          prefix (second (re-find #"^(\d+:) " change))]
      (recur (map #(str/replace % (re-pattern (str prefix ".*$")) change) rules)
             (rest change-rules)))))

(comment (fix ["0: 1" "1: 2" "2: 3"]
              ["0: 2" "1: 2"])
         (fix ["0: 1"] []))

(defn count-valid [file change-rules]
  (let [[rules messages] (map str/split-lines (str/split (slurp file) #"\n\n"))
        fixed-rules (fix rules change-rules)
        grammar (into ["S = 0"] (map #(str/replace % ": " " = ") fixed-rules))
        _ (do (newline)
              (println (str "Rules: " (count grammar)))
              (println (str/join "\n" (map #(str "\"" % "\"") grammar))))
        parser (insta/parser (str/join "\n" grammar))
        valid (remove (fn [msg] (insta/failure? (insta/parse parser msg))) messages)]
    (count valid)))

(def fixes ["8: 42 | 42 8"
            "11: 42 31 | 42 11 31"])

(comment
  (= 2 (count-valid "example" []))
  (= 3 (count-valid "example2" []))
  (= 147 (count-valid "input" []))

  (= (count-valid "example2" fixes)
     12)

  (count-valid "input" fixes)
  )

