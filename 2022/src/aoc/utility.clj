(ns aoc.utility
  (:require
   [babashka.fs :as fs]
   [babashka.curl :as curl]
   [clojure.string :as str]
   [nextjournal.clerk :as clerk]))

(defn file->lines [filename]
  (-> filename slurp str/split-lines))

;; Cribbed from https://github.com/nextjournal/clerk/issues/233#issue-1415414121
(defmacro verify [expression & [message]]
  (let [msg (or message (pr-str expression))
        success-msg (str "✅ " msg)
        error-msg (str "❌ " msg " ;=> ")]
    `(clerk/code
      (try
        (let [ret# ~expression]
          (if ret#
            ~success-msg
            (str ~error-msg (pr-str ret#))))
        (catch Throwable ex#
          (str ~error-msg "Error: " (ex-message ex#)))))))

(defn input-files [prefix]
  (map str (fs/glob "." (str "input/" prefix "*"))))

;; Cribbed from https://github.com/borkdude/advent-of-babashka/blob/main/bb/new_day.clj
(defn fetch-input [& {:keys [day year] :or {year 2022}}]
  (if-let [session (System/getenv "AOC_SESSION")]
    (let [input-url (format "https://adventofcode.com/%d/day/%d/input" year day)
          headers {"Cookie" (str "session=" session)
                   "User-Agent" "github.com/dgtized/advent-of-code dgtized@gmail.com"}]
      (println "retrieving input from " input-url)
      (let [{:keys [status body] :as resp} (curl/get input-url {:headers headers})]
        (if (= 200 status)
          body
          (throw (ex-info "unable to download"
                          {:day day
                           :year year
                           :response resp})))))))

#_(fetch-input :day 7)

(defn day-input [& {:keys [day year name content force]
                    :or {year 2022
                         name "input"
                         force false}}]
  (let [filename (format "input/day%02d.%s" day name)]
    (if (or force (not (fs/exists? filename)))
      (spit filename (or content
                         (fetch-input :day day :year year)))
      (println (str "! " filename " already exists")))))

#_(day-input :day 8)

(defn answer-table [methods files f]
  (clerk/table
   {:head ["input" "star" "answer"]
    :rows
    (for [[star process] (map-indexed vector methods)
          file files]
      (concat [file (inc star)]
              (let [r (f {:file file
                          :star (inc star)
                          :result (process file)})]
                (if (sequential? r)
                  r
                  [r]))))}))

(def grid->table
  {:pred seq
   :transform-fn clerk/mark-presented
   :render-fn
   '(fn [grid]
      (into [:table]
            (for [row grid]
              (into [:tr]
                    (for [n row]
                      [:td
                       {:style {:width 20 :height 20}
                        :class (if (pos? n)
                                 "bg-black"
                                 "bg-white border-grey border-solid border-2")}])))))})
