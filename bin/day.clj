#!/usr/bin/env bb

(ns day
  (:require [babashka.fs :as fs]
            [babashka.curl :as curl]))

(def year 2021)
(def now (java.time.ZonedDateTime/now))
(def day (.getDayOfMonth now))

;; Create todays input if missing
(let [today (format "%4d/day%02d" year day)]
  (when-not (fs/directory? today)
    (fs/create-dirs today))

  ;; TODO handle oauth to fetch input files
  ;; (curl/get "https://adventofcode.com/2021/day/1/input")
  )


