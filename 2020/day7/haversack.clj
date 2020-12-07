(ns haversack
  (:require
   [clojure.java.io :as io]
   [clojure.core.logic :as cl]
   [clojure.string :as str]
   [clojure.core.logic.pldb :as pldb]))

(defn parse-contents [contents]
  (if (= contents "no other bags") {}
      (->> (str/split contents #", ?")
           (map (fn [b]
                  (if-let [[_ n bag] (re-matches #"(\d+) (.* bag)s?" b)]
                    {bag (read-string n)})))
           (into {}))))

(defn load-rules [file]
  (with-open [rdr (io/reader file)]
    (into {} (for [line (line-seq rdr)]
               (let [m (re-seq #"(.* bag)s contain (.*)." line)]
                 (if-let [[bag contents] (nfirst m)]
                   {bag (parse-contents contents)}
                   m))))))

(defn contained-by [rules]
  (mapcat (fn [[container contents]]
            (if (seq contents)
              (for [bag (keys contents)]
                [bag container])
              [[container nil]]))
          rules))

(pldb/db-rel bag b)
(pldb/db-rel contains ^:index b1 ^:index b2)
;; (pldb/db-rel contains-none b)

(defn facts [rules]
  (mapcat
   (fn [[b c]] (if c
                [[bag b]
                 [bag c]
                 [contains b c]]
                [[bag b]]))
   rules))

(def example-facts (apply pldb/db (facts (contained-by (load-rules "example")))))
(def input-facts (apply pldb/db (facts (contained-by (load-rules "input")))))

(defn containo [bag goal]
  (cl/conde
   [(contains bag goal)]
   [(cl/fresh [x]
      (contains bag x)
      (containo x goal))]))

(defn all-contain [db goal]
  (pldb/with-db db
    (cl/run* [bag]
      (containo goal bag))))

(comment
  (->> (all-contain example-facts "shiny gold bag")
       distinct
       count)
  (->> (all-contain input-facts "shiny gold bag")
       distinct
       count))
