(ns seven-segment-search
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-line [line]
  (map (fn [s] (map sort (str/split s #"\s+"))) (str/split line #"\|\s+")))

(defn parse [filename]
  (mapv parse-line (str/split-lines (slurp filename))))

;; Canonical representation:
;;  aaaa
;; b    c
;; b    c
;;  dddd
;; e    f
;; e    f
;;  gggg

(def known-digits
  {2 1 ;; cf
   3 7 ;; acf
   4 4 ;; bcdf
   7 8 ;; abcdefg
   })

;; 9 [abcdfg] 6 missing e
;; 6 [abdefg] 6 missing c
;; 5 [abdfg]  5 missing c,e
;; 3 [acdfg]  5 missing b,e
;; 2 [acdeg]  5 missing b,f
;; 0 [abcefg] 6 missing d

(defn part1 [input]
  (let [freqs (->> input
                   (map second)
                   (mapcat #(map (comp known-digits count) %))
                   frequencies)]
    (apply + (vals (dissoc freqs nil)))))

(assert (= 26 (part1 (parse "example"))))
(assert (= 352 (part1 (parse "input"))))

(def ex2 (parse-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"))

(defn solve-digits [digits]
  (let [base (zipmap digits
                     (for [digit digits
                           :let [c (count digit)]]
                       (set (if-let [k (known-digits c)]
                              [k]
                              (if (= c 5)
                                [2 3 5]
                                [0 6 9])))))]
    base))

(defn cases-matching [base values]
  (keep (fn [[in possible]]
          (when (set/superset? (set values) possible) in))
        base))

(assert (= 1 (count (cases-matching (solve-digits (first ex2)) #{7}))))

(defn missing-segments [digits]
  (set/difference (set "abcdefg") (set digits)))

(defn disjoint-from-missing-segments [a]
  (fn [in]
    (when (empty? (set/intersection a (missing-segments in)))
      in)))

(defn solve-0 [base]
  (let [missing-235 (->> (cases-matching base #{2 3 5})
                         (map missing-segments)
                         (apply set/union))]
    (some (disjoint-from-missing-segments missing-235)
          (cases-matching base #{0 6 9}))))

(defn solve-5 [base]
  (let [missing-ec (->> (cases-matching base #{6 9})
                        (map missing-segments)
                        (apply set/union))]
    (some (fn [in]
            (when (= (missing-segments in) missing-ec)
              in))
          (cases-matching base #{2 3 5}))))

(defn solve-2 [base]
  (let [missing-five (missing-segments (first (cases-matching base #{5})))]
    (some (disjoint-from-missing-segments missing-five)
          (cases-matching base #{2 3}))))

(defn solve-6 [base]
  (let [missing-three (missing-segments (first (cases-matching base #{3})))]
    (some (disjoint-from-missing-segments missing-three)
          (cases-matching base #{6 9}))))

(defn prove [base digit value]
  (assoc (zipmap (keys base) (map #(disj % value) (vals base)))
         digit (set [value])))

(defn solution [digits]
  (let [base (solve-digits digits)
        zero (solve-0 base)
        base0 (prove base zero 0)
        five (solve-5 base0)
        base5 (prove base0 five 5)
        two (solve-2 base5)
        base2 (prove base5 two 2)
        mapping (prove base2 (solve-6 base2) 6)]
    (zipmap (keys mapping) (map first (vals mapping)))))

(comment (solution (first ex2)))

(defn solve-line [[digits output]]
  (let [mapping (solution digits)
        s (apply str (map mapping output))]
    ;; edn/read-string doesn't like leading zeros so strip them
    (edn/read-string (str/replace-first s #"^[0]+" ""))))

(assert (= '(8394 9781 1197 9361 4873 8418 4548 1625 8717 4315)
           (map solve-line (parse "example"))))

(defn part2 [input]
  (apply + (map solve-line input)))

(assert (= 61229 (part2 (parse "example"))))
(assert (= 936117 (part2 (parse "input"))))

