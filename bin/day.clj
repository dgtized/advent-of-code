#!/usr/bin/env bb
;; adapted from https://github.com/borkdude/advent-of-babashka/blob/main/bb/new_day.clj
(ns day
  (:require [clojure.string :as str]
            [babashka.fs :as fs]
            [babashka.cli :as cli]
            [babashka.curl :as curl]))

(defn snake-case [s]
  (str/replace s #"\-" "_"))

(defn kebab-case [s]
  (str/replace s #"_" "-"))

(defn generate [template-file {:keys [year day problem]}]
  (let [template (slurp template-file)
        day-template (format "%4d/src/day%02d/%s.clj"
                             year day (snake-case problem))]
    (println "generating" day-template)
    (if-not (fs/exists? day-template)
      (spit day-template
            (-> template
                (str/replace #"day" (format "day%02d" day))
                (str/replace #"template" (kebab-case problem))))
      (println day-template "already exists"))))

(defn fetch-input [{:keys [year day]}]
  (if-let [session (System/getenv "AOC_SESSION")]
    (let [url (format "https://adventofcode.com/%4d/day/%d/input" year day)
          day-input (format "%4d/src/day%02d/input" year day)]
      (println "fetching " url " -> " day-input)
      (if-not (fs/exists? day-input)
        (let [headers
              {"Cookie" (str "session=" session)
               "User-Agent" "github.com/dgtized/advent-of-code by dgtized@gmail.com"}
              body (:body (curl/get url {:headers headers}))]
          (spit day-input body))
        (println day-input " already downloaded")))
    (println "Set AOC_SESSION to download input")))

(defn build-day [{:keys [year day] :as opts}]
  (let [today (format "%4d/src/day%02d" year day)]
    (if-not (fs/directory? today)
      (do (println (format "building %d - %02d" year day))
          (fs/create-dirs today)
          (fetch-input opts)
          (generate (format "%d/template.clj" year)
                    (merge {:problem (format "day%02d" day)} opts)))
      (println today "already exists"))))

(defn advent-day? [day]
  (<= 1 day 25))

(def cli-spec
  {:spec
   {:year {:coerce :long :desc "Year" :alias :y}
    :day {:coerce :long :desc "Day" :alias :d :validate advent-day?}
    :problem {:desc "Problem Name" :alias :n}}
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
