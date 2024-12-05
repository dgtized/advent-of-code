#!/usr/bin/env bb

(ns day
  (:require [babashka.fs :as fs]
            [babashka.cli :as cli]
            [babashka.curl :as curl]))

(defn build-day [{:keys [year day]}]
  (println (format "setup %d - %02d" year day))
  (let [today (format "%4d/src/day%02d" year day)]
    (when-not (fs/directory? today)
      (println (format "building %s" today))
      (fs/create-dirs today))

    ;; TODO handle oauth to fetch input files
    ;; (curl/get "https://adventofcode.com/2021/day/1/input")
    ))

(defn advent-day? [day]
  (<= 1 day 25))

(def cli-spec
  {:spec
   {:year {:coerce :long :desc "Year" :alias :y}
    :day {:coerce :long :desc "Day" :alias :d :validate advent-day?}}
   :error-fn
   (fn [{:keys [type cause msg option]}]
     (when (= :org.babashka/cli type)
       (case cause
         :require
         (println
          (format "Missing required argument: %s\n" option))
         :validate
         (do (println
              (format "%s does not exist!\n" msg))
             (System/exit 1)))))})

(defn help-args [spec]
  (println "Usage: bin/day.clj [opts]")
  (println (cli/format-opts (merge spec {:order (vec (keys (:spec spec)))}))))

;; Create todays input if missing
(defn -main [args]
  (let [now (java.time.ZonedDateTime/now)
        opts (cli/parse-opts args cli-spec)]
    (if (or (:help opts) (:h opts))
      (help-args cli-spec)
      (build-day (merge {:year (.getYear now)
                         :day (.getDayOfMonth now)} opts)))))

(-main *command-line-args*)
