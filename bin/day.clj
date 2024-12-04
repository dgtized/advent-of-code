#!/usr/bin/env bb

(ns day
  (:require [babashka.fs :as fs]
            [babashka.curl :as curl]))

(def now (java.time.ZonedDateTime/now))
(def year (.getYear now))
(def day (.getDayOfMonth now))

(defn build-day [year day]
  (let [today (format "%4d/src/day%02d" year day)]
    (when-not (and (fs/directory? today) (<= 1 day 25))
      (println (format "building %s" today))
      (fs/create-dirs today))

    ;; TODO handle oauth to fetch input files
    ;; (curl/get "https://adventofcode.com/2021/day/1/input")
    ))

;; Create todays input if missing
(build-day year day)
(build-day year (inc day))
