(ns aoc.utility
  (:require
   [babashka.curl :as curl]
   [babashka.fs :as fs]
   [clojure.data.priority-map :as dpm]
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

(defmacro with-time
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [ret# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]))

(defn answer-table [methods files f]
  (clerk/table
   {:head ["input" "star" "ms" "answer"]
    :rows
    (for [[star process] (map-indexed vector methods)
          file files
          :let [[result duration] (with-time (process file))]]
      (concat [file (inc star) (format "%.1f" duration)]
              (let [r (f {:file file
                          :star (inc star)
                          :result result
                          :duration duration})]
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

(defn split-empty-lines [s]
  (str/split s #"\n\n"))

(defn parse-ints [line]
  (mapv parse-long (re-seq #"\d+" line)))

;; originally adapted from https://github.com/arttuka/astar/blob/master/src/astar/core.cljc
(defn backtrack [current visited]
  (cons current
        (lazy-seq (when-let [parent (get visited current)]
                    (backtrack parent visited)))))

(defn a*-search
  ([successors source target]
   (a*-search {} successors source target))
  ([{:keys [cost heuristic goal?] :or
     {cost (constantly 1)
      heuristic (constantly 1)
      goal? =}}
    successors source target]
   (loop [visited {}
          queue (dpm/priority-map-keyfn first source [0 0 nil])]
     (when (seq queue)
       (let [[current [_ value prev]] (peek queue)
             visited' (assoc visited current prev)]
         ;; (println current)
         (if (goal? current target)
           (reverse (backtrack current visited'))
           (recur visited'
                  (reduce (fn [queue node]
                            (let [score (+ value (cost current node))]
                              (if (and (not (contains? visited' node))
                                       (or (not (contains? queue node))
                                           (< score (get-in queue [node 1]))))
                                (assoc queue node [(+ score (heuristic node)) score current])
                                queue)))
                          (pop queue)
                          (successors current)))))))))
