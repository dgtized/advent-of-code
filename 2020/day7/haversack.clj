(ns haversack
  (:require
   [clojure.java.io :as io]
   [clojure.core.logic :as cl]
   [clojure.string :as str]
   [clojure.core.logic.pldb :as pldb]))

(pldb/db-rel contains ^:index c ^:index b quantity)
(pldb/db-rel empty-bag b)

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
           [empty-bag container]
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

(comment
  (->> (all-contain (make-db "example") "shiny gold bag")
       distinct
       count)
  (->> (all-contain (make-db "input") "shiny gold bag")
       distinct
       count))
