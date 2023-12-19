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

(defn limit [accepted {:keys [k f v d]}]
  (if (= d "R")
    (if (= f "<")
      (update accepted k (fn [[l u]] [(max l v) u]))
      (update accepted k (fn [[l u]] [l (min (inc v) u)])))
    (if (= f "<")
      (update accepted k (fn [[l u]] [l (min (inc v) u)]))
      (update accepted k (fn [[l u]] [(max l v) u])))))

(defn constrain [rule-set queue accepted]
  (let [rules (get rule-set queue)]
    (println "at" queue accepted)
    (cond (= queue "A")
          (let [s (apply * (map (fn [[l h]] (let [d (- h l)]
                                             (if (< d 0) 0 d)))
                                (vals accepted)))]
            (println s)
            s)
          (= queue "R")
          0
          :else
          (do (println rules)
              (apply +
                     (map
                      (fn [{:keys [f d] :as r}]
                        (if f
                          (constrain rule-set d (limit accepted r))
                          (constrain rule-set d accepted)))
                      rules))))))

(defn part2 [{:keys [rules]}]
  (println)
  (constrain rules "in"
             {"x" [1 4001] "m" [1 4001]
              "a" [1 4001] "s" [1 4001]}))
(assert (= 167409079868000 (part2 (parse example))))
;; (assert (= (part2 (parse input))))
