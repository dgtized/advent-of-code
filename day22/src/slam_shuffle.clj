(ns slam-shuffle)

(defn read-program [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn new-deck []
  (range 0 10007))

(defn deal-increment [increment deck]
  (let [deck-size (count deck)]
    (apply assoc
           (vec deck)
           (flatten
            (map-indexed (fn [idx card]
                           [(mod (* idx 3) deck-size) card])
                         deck)))))

;; inc 3, deck 5
;; 0 -> i(0) * 3 = 0
;; 1 -> i(1) * 3
;; 2 -> i(2) * 3 = 6-5 = 1
;; 3 -> i(3) * 3 = 9-5 = 4
;; 4 -> i(4) * 3 = 12-5-5 = 2
(comment (deal-increment 3 (range 0 5)))

(defn interpret [command deck]
  (cond (= "deal into new stack" command)
        (reverse deck)
        (re-find #"cut" command)
        (let [n (->> command
                     (re-matches #"cut (-?\d+)")
                     last
                     read-string)]
          (if (> n 0)
            (concat (drop n deck) (take n deck))
            (concat (take-last (Math/abs n) deck)
                    (drop-last (Math/abs n) deck))))
        (re-find #"deal with increment" command)
        (let [n (->> command
                     (re-matches #"with increment (-?\d+)")
                     last
                     read-string)]
          (deal-increment 0 n deck))
        :else deck))

(comment
  (interpret "deal into new stack" (range 0 5))
  (interpret "cut 2" (range 0 5))
  (interpret "cut -2" (range 0 5)))

(defn run [program deck]
  (reduce (fn [new-deck cmd] (interpret cmd new-deck))
          deck program))


(comment
  (run ["cut 2" "deal into new stack"] (range 0 9)))

(comment
  (run (read-program "input") (new-deck)))
