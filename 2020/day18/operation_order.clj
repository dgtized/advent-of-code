(ns operation-order
  (:require [instaparse.core :as insta]
            [clojure.string :as str]))

(def left-parser (insta/parser "P = expr
<expr> = parens | add | multiply | number
add = expr <whitespace> <'+'> <whitespace> (number | parens)
multiply = expr <whitespace> <'*'> <whitespace> (number | parens)
<parens> = <'('> <whitespace> expr <whitespace> <')'>
whitespace = #'\\s*'
number = #'[\\d]+'
"))

(defn read-eval [line]
  (let [tree (left-parser line)
        transformations
        {:P identity
         :number read-string
         :add +
         :multiply *}]
    (insta/transform transformations tree)))

(defn first-star [filename]
  (->> filename
       slurp
       str/split-lines
       (map read-eval)
       (reduce +)))

(comment
  (first-star "example")
  (first-star "input")

  (insta/parses left-parser "1 + 2 * 3")
  (insta/parses left-parser "1 * 2 + 3")
  (insta/parses left-parser "1 + (2 * 3)")
  (insta/parses left-parser "(1 + 2) + 2")
  (insta/parses left-parser "2 * 3 + (4 * 5)")
  (insta/parses left-parser "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  )

