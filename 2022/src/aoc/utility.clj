(ns aoc.utility)

;; Cribbed from https://github.com/nextjournal/clerk/issues/233#issue-1415414121
(defmacro verify [expression & [message]]
  (let [msg (or message (pr-str expression))
        success-msg (str "✅ " msg)
        error-msg (str "❌ " msg " ;=> ")]
    `(try
       (let [ret# ~expression]
         (if ret#
           ~success-msg
           (str ~error-msg (pr-str ret#))))
       (catch Throwable ex#
         (str ~error-msg "Error: " (ex-message ex#))))))
