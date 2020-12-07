(ns haversack
  (:require
   [clojure.java.io :as io]
   [clojure.core.logic :as cl]
   [clojure.core.logic.fd :as fd]
   [clojure.string :as str]
   [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel contains ^:index c ^:index b quantity)

(defn parse-contents [contents]
  (if (= contents "no other bags") [[nil 0]]
      (->> (str/split contents #", ?")
           (map (fn [b]
                  (if-let [[_ n bag] (re-matches #"(\d+) (.* bag)s?" b)]
                    [bag (read-string n)])))
           (into []))))

(defn load-rules [file]
  (with-open [rdr (io/reader file)]
    (->> (for [line (line-seq rdr)]
           (let [m (re-seq #"(.* bag)s contain (.*)." line)]
             (if-let [[bag contents] (nfirst m)]
               (map (partial cons bag) (parse-contents contents))
               m)))
         doall
         (apply concat)
         vec)))

(defn facts [rules]
  (map (fn [[container bag quantity]]
         (if (zero? quantity)
           [contains container "" 0]
           [contains container bag quantity]))
       rules))

(defn make-db
  [file]
  (->> (load-rules file)
       facts
       (apply pldb/db)))

(defn containo [bag goal]
  (cl/fresh [q]
    (cl/conde
     [(contains bag goal q)]
     [(cl/fresh [x]
        (contains bag x q)
        (containo x goal))])))

(defn all-contain [db goal]
  (pldb/with-db db
    (cl/run* [bag]
      (containo bag goal))))

(defn part1 [file goal]
  (->> (all-contain (make-db file) goal)
       distinct
       count))

(defn sumo [container sum]
  (cl/conde
   [(contains container "" 0)
    (cl/== sum 0)]
   [(cl/fresh [x q t]
      (contains container x q)
      (sumo x t)
      (fd/+ q t sum))]))

(defn part2 [file start]
  (pldb/with-db (make-db file)
    (cl/run* [total]
      (sumo start total))))

(comment
  (part1 "example" "shiny gold bag") ;; 4
  (part1 "input" "shiny gold bag") ;; 185
  (part2 "example" "shiny gold bag") ;; 32
  (part2 "example2" "shiny gold bag") ;; 126
  (part2 "input" "shiny gold bag"))
