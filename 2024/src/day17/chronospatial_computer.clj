(ns day17.chronospatial-computer
  (:require [clojure.math :as math]
            [clojure.string :as str]))

(def input (slurp "src/day17/input"))
(def example (slurp "src/day17/example"))

(defn parse [in]
  (let [input (mapv parse-long (re-seq #"\d+" in))
        [a b c] (take 3 input)]
    {:registers {:a a :b b :c c}
     :mode :run
     :ip 0
     :program (mapv biginteger (drop 3 input))
     :out []}))

(defn combo-op [state op]
  (case op
    0 0
    1 1
    2 2
    3 3
    4 (get-in state [:registers :a])
    5 (get-in state [:registers :b])
    6 (get-in state [:registers :c])))

(defn advance [state]
  (update state :ip + 2))

(defn evaluate [{:keys [registers ip program] :as state}]
  (if (>= ip (count program))
    (assoc state :mode :halt)
    (let [op (nth program ip)
          literal (nth program (inc ip))]
      (case op
        ;; adv
        0 (-> state
              (assoc-in [:registers :a]
                        (biginteger (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
              advance)
        ;; bxl
        1 (-> state
              (assoc-in [:registers :b]
                        (.xor (biginteger (get registers :b)) (biginteger literal)))
              advance)
        ;; bst
        2 (-> state
              (assoc-in [:registers :b]
                        (mod (combo-op state literal) 8))
              advance)
        ;; jnz
        3 (if (zero? (get registers :a))
            (advance state)
            (assoc state :ip literal))
        ;; bxc
        4 (-> state
              (assoc-in [:registers :b]
                        (.xor (biginteger (get registers :b))
                              (biginteger (get registers :c))))
              advance)
        ;; out
        5 (-> state
              (update :out conj (mod (combo-op state literal) 8))
              advance)
        ;; bdv
        6 (-> state
              (assoc-in [:registers :b]
                        (biginteger (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
              advance)
        7 (-> state
              (assoc-in [:registers :c]
                        (biginteger (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
              advance)
        ))))

(defn run [state]
  (take-while (fn [{:keys [mode]}] (= mode :run))
              (iterate evaluate state)))

(defn run-all [state]
  (first (drop-while (fn [{:keys [mode]}] (= mode :run))
                     (iterate evaluate state))))

(assert (= 1 (get-in (evaluate {:registers {:a 0 :b 0 :c 9}
                                :ip 0
                                :mode :run
                                :program [2 6]})
                     [:registers :b])))

(assert (= [0 1 2] (:out (run-all {:registers {:a 10 :b 0 :c 9}
                                   :ip 0
                                   :mode :run
                                   :program [5 0 5 1 5 4]
                                   :out []}))))

(let [{:keys [out registers]} (run-all {:registers {:a 2024 :b 0 :c 9}
                                        :ip 0
                                        :mode :run
                                        :program [0 1 5 4 3 0]
                                        :out []})]
  (assert (and (= [4 2 5 6 7 7 7 7 3 1 0] out)
               (= 0 (registers :a)))))

(assert (= 26 (get-in (evaluate {:registers {:a 0 :b 29 :c 0}
                                 :ip 0
                                 :mode :run
                                 :program [1 7]})
                      [:registers :b])))

(assert (= 44354 (get-in (evaluate {:registers {:a 0 :b 2024 :c 43690}
                                    :ip 0
                                    :mode :run
                                    :program [4 0]})
                         [:registers :b])))

(defn part1 [state]
  (str/join "," (:out (run-all state))))

(assert (= "4,6,3,5,6,3,5,2,1,0" (part1 (parse example))))
(assert (= "7,5,4,3,4,5,3,4,6" (part1 (parse input))))

(defn quine? [{:keys [program] :as state} init]
  (let [state' (assoc-in state [:registers :a] init)
        steps (run state')
        result (update (last steps) :out (partial mapv int))]
    [(= (:out result) program) init (count steps) (:out result)]))

(defn part2 [state start limit report]
  (println [start limit])
  (loop [init start steps 0 len 0]
    (let [r (quine? state init)]
      (when (not= (nth r 2) steps)
        (println [:step r]))
      (when  (not= (count (nth r 3)) len)
        (println [:len r]))
      (when (and (pos? report) (= (mod init report) 0))
        (println [:report r]))
      (when (= (first (:program state))
               (first (:out r)))
        (println [:match r]))
      (if (or (first r) (> init (+ start limit)))
        r
        (recur (inc init) (nth r 2) (count (nth r 3)))))))

(assert (= [true 117440 19 [0 3 5 4 3 0]]
           (quine? {:registers {:a 2024 :b 0 :c 0} :mode :run :out [] :ip 0 :program [0 3 5 4 3 0]}  117440)))
;; (count [2 4 1 1 7 5 1 5 4 3 5 5 0 3 3 0]) => 16

(comment (part2 (parse input) 0 (int (math/pow 2 18)) 0)
         [:step [false 0 9 [4]]]
         [:len [false 0 9 [4]]]
         [:step [false 8 17 [0 4]]]
         [:len [false 8 17 [0 4]]]
         [:step [false 64 25 [4 0 4]]]
         [:len [false 64 25 [4 0 4]]]
         [:step [false 512 33 [4 4 0 4]]]
         [:len [false 512 33 [4 4 0 4]]]
         [:step [false 4096 41 [4 4 4 0 4]]]
         [:len [false 4096 41 [4 4 4 0 4]]]
         [:step [false 32768 49 [4 4 4 4 0 4]]]
         [:len [false 32768 49 [4 4 4 4 0 4]]]
         [:step [false 262144 57 [4 4 4 4 4 0 4]]]
         [:len [false 262144 57 [4 4 4 4 4 0 4]]])

(comment (for [i (range 12)]
           [i (biginteger (math/pow 8 i))])
         ([0 1]
          [1 8]
          [2 64]
          [3 512]
          [4 4096]
          [5 32768]
          [6 262144]
          [7 2097152]
          [8 16777216]
          [9 134217728]
          [10 1073741824]
          [11 8589934592]))

(biginteger (math/pow 8 16))

;; [2 4 1 1 7 5 1 5 4 3 5 5 0 3 3 0]
(assert (= (part2 (parse input) (biginteger (+ (math/pow 8 15) (math/pow 8 10))) 512 1)))


;; (quine? (parse input) 24117513)

;; (bigint (math/pow 2 ))

;; => [false 2411751543 89 [2 0 4 1 2 4 4 2 1 4 6]]
;; => [false 2411751544 89 [0 7 4 1 2 4 4 2 1 4 6]]
