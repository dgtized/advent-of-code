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
     :program (vec (drop 3 input))
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
                        (int (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
              advance)
        ;; bxl
        1 (-> state
              (assoc-in [:registers :b]
                        (bit-xor (get registers :b) literal))
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
                        (bit-xor (get registers :b)
                                 (get registers :c)))
              advance)
        ;; out
        5 (-> state
              (update :out conj (mod (combo-op state literal) 8))
              advance)
        ;; bdv
        6 (-> state
              (assoc-in [:registers :b]
                        (int (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
              advance)
        7 (-> state
              (assoc-in [:registers :c]
                        (int (/ (get registers :a) (math/pow 2 (combo-op state literal)))))
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

(defn part2 [in]
  in)

(assert (= (part2 (parse example))))
(assert (= (part2 (parse input))))
