(ns day24.crossed-wires
  (:require [clojure.string :as str]))

(def input (slurp "src/day24/input"))
(def example (slurp "src/day24/example"))
(def example2 (slurp "src/day24/example2"))

(defn parse [in]
  (let [[wires gates] (str/split in #"\n\n")]
    {:wires (into {} (map (fn [s] (let [[wire v] (str/split s #": ")]
                                   [wire (parse-long v)]))
                          (str/split-lines wires)))
     :gates (mapv rest (re-seq #"(.{3,3}) (AND|XOR|OR) (.{3,3}) -> (.{3,3})" gates))}))

(defn evaluate [wires [a op b c]]
  (let [av (get wires a)
        bv (get wires b)
        cv (get wires c)]
    (when (and (not cv) av bv)
      (let [r [c ((case op
                    "AND" bit-and
                    "XOR" bit-xor
                    "OR" bit-or) av bv)]]
        r))))

(defn run [{:keys [wires gates]}]
  (loop [gates gates wires wires]
    (if (empty? gates)
      wires
      (let [resolutions (keep (partial evaluate wires) gates)
            results (set (map first resolutions))]
        (recur (remove (fn [[_ _ _ out]] (contains? results out)) gates)
               (reduce (fn [acc [out v]] (assoc acc out v)) wires resolutions))))))


(defn wire [prefix digit]
  (format "%s%02d" prefix digit))

(defn bit-string [xs]
  (apply str xs))

(defn dec-value [xs]
  (BigInteger. (bit-string xs) 2))

(defn bit-value [wires]
  (for [d (range 63 -1 -1)
        :let [w (wire "z" d)
              v (get wires w)]
        :when v]
    v))

(defn part1 [in]
  (dec-value (bit-value (run in))))

(assert (= 4 (part1 (parse example))))
(assert (= 2024 (part1 (parse example2))))
(assert (= 52038112429798 (part1 (parse input))))

(defn get-wire [input wire]
  (get-in input [:wires wire]))

(defn set-wire [input wire value]
  (assoc-in input [:wires wire] value))

(defn delta [xs]
  (apply str (apply map (fn [& vs]
                          (if (= (count (set vs)) 1)
                            " " "1"))
                    xs)))

(defn zeroed [in]
  (update in :wires (fn [wires]
                      (into {} (for [[w v] wires]
                                 [w 0])))))

(defn part2 [in]
  (for [d (range 64)
        :let [x-wire (wire "x" d)
              y-wire (wire "y" d)
              z-wire (wire "z" d)]
        :when (get-wire in x-wire)
        :let [place (if (zero? d)
                      in
                      (-> in
                          (set-wire x-wire 0)
                          (set-wire y-wire 0)))]]
    [z-wire
     (let [rset (into {}
                      (for [var [x-wire y-wire]
                            :let [v (get-wire in var)]
                            :when v
                            xv [0 1]
                            yv [0 1]
                            :let [r (run (-> place
                                             (set-wire x-wire xv)
                                             (set-wire y-wire yv)))]]
                        [[var xv yv]
                         (bit-string (bit-value r))]))]
       (assoc rset "  delta  " (delta (vals rset))))]))

(assert (= (part2 (zeroed (parse example)))))
(assert (= (part2 (zeroed (parse example2)))))
(assert (= (part2 (zeroed (parse input)))))

;; from inspection, appears that
;; z11,z15, z16?, z23 have carry errors?

(assert (= (part2 (parse example))))
(assert (= (part2 (parse example2))))
(assert (= (part2 (parse input))))


(defn gates-graph [{:keys [gates]}]
  (into {}
        (for [[a op b c] gates]
          [c {:op op :in #{a b}}])))

(gates-graph (parse example2))

(defn dep-graph [graph wire depth]
  (let [node (assoc (get graph wire) :out wire)]
    (if (zero? depth)
      node
      (assoc node
             :child (for [child (:in node)
                          :when (get graph child)]
                      (dep-graph graph child (dec depth)))))))

(dep-graph (gates-graph (parse example2)) "z02" 5)

(def graph (gates-graph (parse input)))
(dep-graph graph "z03" 3) ;; =>
{:op "XOR",
 :in #{"ptp" "tcb"},
 :out "z03",
 :child
 ({:op "OR",
   :in #{"npq" "qvc"},
   :out "ptp",
   :child
   ({:op "AND",
     :in #{"prf" "dvr"},
     :out "npq",
     :child
     ({:op "OR", :in #{"vpp" "svv"}, :out "prf"}
      {:op "XOR", :in #{"y02" "x02"}, :out "dvr"})}
    {:op "AND", :in #{"y02" "x02"}, :out "qvc", :child ()})}
  {:op "XOR", :in #{"x03" "y03"}, :out "tcb", :child ()})}

;; should be xor(xor(y(d),x(d)), or(and(x(d-1), y(d-1)), and(carry(d-1), carry(d-1))))
(dep-graph graph "z09" 2)
(dep-graph graph "z10" 2)
(dep-graph graph "z11" 2)
(dep-graph graph "z12" 2) ;; swap with xor? kwb,z12?
(dep-graph graph "z13" 2)

(defn swap [{:keys [gates] :as in} a b]
  (let [ga (some (fn [gate] (when (= (last gate) a) gate)) gates)
        gb (some (fn [gate] (when (= (last gate) b) gate)) gates)]
    (update in :gates
            (fn [gates] (replace {ga (replace {b a} gb)
                                 gb (replace {a b} ga)}
                                gates)))))

(def input1 (swap (parse input) "kwb" "z12"))

(part2 (zeroed input1))
(def graph1 (gates-graph (zeroed input1)))


(dep-graph graph1 "z13" 2)
(dep-graph graph1 "z14" 2)
(dep-graph graph1 "z15" 2) ;; =>
{:op "XOR",
 :in #{"crj" "kfw"},
 :out "z15",
 :child
 ({:op "OR",
   :in #{"djq" "pjp"},
   :out "crj",
   :child
   ({:op "AND", :in #{"y14" "x14"}, :out "djq"}
    {:op "AND", :in #{"jqt" "wmg"}, :out "pjp"})}
  {:op "XOR", :in #{"x15" "y15"}, :out "kfw", :child ()})}

(dep-graph graph1 "z16" 2) ;; and at root in stead of xor ;; z16 / qkf
(dep-graph graph1 "z17" 3)

(def input2 (swap input1 "qkf" "z16"))

(def graph2 (gates-graph (zeroed input2)))
(part2 (zeroed input2))

(dep-graph graph2 "z22" 2)
(dep-graph graph2 "z23" 2)
(dep-graph graph2 "z24" 2) ;; Z24 / tgr ;; 
(dep-graph graph2 "z25" 2)

(def input3 (swap input2 "tgr" "z24"))
(def graph3 (gates-graph (zeroed input3)))

(part2 (zeroed input3))

(for [d (range 45)]
  [d (sort (map :op (tree-seq :child :child (dep-graph graph3 (wire "z" d) 2))))])

(dep-graph graph3 "z28" 2)
(dep-graph graph3 "z29" 2)
(dep-graph graph3 "z30" 2)
(dep-graph graph3 "z31" 2)

(def input4 (swap input3 "cph" "jqn"))
(def graph4 (gates-graph (zeroed input4)))

(for [d (range 45)]
  [d (sort (map :op (tree-seq :child :child (dep-graph graph4 (wire "z" d) 2))))])

(dep-graph graph4 "z28" 2)
(dep-graph graph4 "z29" 2)
(dep-graph graph4 "z30" 2)
(dep-graph graph4 "z31" 2)


(assert (= "cph,jqn,kwb,qkf,tgr,z12,z16,z24"
           (str/join "," (sort ["kwb" "z12" "qkf" "z16" "tgr" "z24" "cph" "jqn"]))))
