#!/usr/bin/env bb

(ns leaderboard
  (:require [clojure.java.io :as io]
            [cheshire.core :as json])
  (:import 'java.time.format.DateTimeFormatter
           'java.time.Instant
           'java.time.LocalDateTime
           'java.time.ZoneId))

(defn fetch-leaderboard []
  (-> (io/file "board.json")
      slurp
      (json/parse-string true)))

(defn date-format [inst]
  (let [f (DateTimeFormatter/ofPattern "YY-MM-dd HH:mm:ss")]
    (.format f (-> inst
                   json/parse-string
                   Instant/ofEpochSecond
                   (LocalDateTime/ofInstant (ZoneId/of "-6"))))))

(defn completions [scores]
  (for [[level stars] (:completion_day_level scores)]
    (into [(json/parse-string (name level))]
          (map second (sort-by first (for [[star date] stars
                                           :let [inst (-> date :get_star_ts date-format)]]
                                       [star inst]))))))

(defn users []
  (for [[_ scores] (:members (fetch-leaderboard))]
    (assoc (dissoc scores :completion_day_level :last_star_ts)
           :last-star (date-format (:last_star_ts scores))
           :scores
           (into [] (completions scores)))))

(users)

