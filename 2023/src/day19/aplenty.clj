(ns day19.aplenty
  (:require [clojure.string :as str]))

(def input (slurp "src/day19/input"))
(def example (slurp "src/day19/example"))

(def f->cond
  {"<" < ">" >})

(defn parse-rule [rule]
  (let [[workflow conditions] (rest (re-find #"([^{]+)\{(.*)\}" rule))]
    {workflow
     (for [condition (str/split conditions #",")]
       (if-let [m (re-find #"([^:]+):(.*)" condition)]
         (let [[trigger dest] (rest m)
               [key f val] (rest (re-find #"([^<>]+)([<>])(.*)" trigger))]
           {:k key :f f :v (parse-long val) :d dest})
         {:d condition}))}))

(defn parse-part [part]
  (into {}
        (for [c (-> part
                    (str/replace #"[{}]" "")
                    (str/split #","))]
          (let [[k v] (str/split c #"=")]
            [k (parse-long v)]))))

(defn parse [in]
  (let [[rules parts] (str/split in #"\n\n")]
    {:rules (into {} (mapv parse-rule (str/split-lines rules)))
     :parts (mapv parse-part (str/split-lines parts))}))

(defn run [rule-set [path part]]
  (if-let [rules (get rule-set (last path))]
    (let [dest (if-let [dest (some (fn [{:keys [k f v d]}]
                                     (when (and f ((f->cond f) (part k) v))
                                       d)) rules)]
                 dest
                 (:d (last rules)))]
      [(conj path dest) part])
    [path part]))

(defn workflow [rules part]
  (->> [["in"] part]
       (iterate (fn [s] (run rules s)))
       (drop-while (fn [[p _]] (not (contains? #{"A" "R"} (last p)))))
       ffirst))

(defn part1 [{:keys [rules parts]}]
  (apply + (for [part parts
                 :let [r (last (workflow rules part))]
                 :when (= r "A")]
             (apply + (vals part)))))

(assert (= 19114 (part1 (parse example))))
(assert (= 263678 (part1 (parse input))))

(defn limit [accepted {:keys [k f v]}]
  (if (= f "<")
    [(update accepted k (fn [[l _]] [l v]))
     (update accepted k (fn [[_ u]] [v u]))]
    [(update accepted k (fn [[_ u]] [(inc v) u]))
     (update accepted k (fn [[l _]] [l (inc v)]))]))

(defn score [accepted]
  (apply * (map (fn [[l h]] (- h l))
                (vals accepted))))

(defn constrain [rule-set flow accepted]
  (let [rules (get rule-set flow)]
    (println "at" flow accepted)
    (cond (= flow "A")
          (score accepted)
          (= flow "R")
          0
          :else
          (do (println rules)
              (first
               (reduce
                (fn [[total accepted] {:keys [f d] :as r}]
                  (if f
                    (let [[acc rej] (limit accepted r)]
                      [(+ total (constrain rule-set d acc)) rej])
                    [(+ total (constrain rule-set d accepted)) accepted]))
                [0 accepted]
                rules))))))

(defn part2 [{:keys [rules]}]
  (println)
  (constrain rules "in"
             {"x" [1 4001] "m" [1 4001]
              "a" [1 4001] "s" [1 4001]}))

(assert (= 167409079868000 (part2 (parse example))))
(assert (= 125455345557345 (part2 (parse input))))
