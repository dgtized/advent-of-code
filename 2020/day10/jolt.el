(require 's)
(require 'f)
(require 'dash)

(defun read-adapters (filename)
  (sort (mapcar #'string-to-number
                (s-lines (s-trim (f-read-text filename))))
        #'<))

(defun jolt-differences (adapters)
  (cl-loop for a in (cons 0 adapters)
           for b in adapters
           collect (- b a)))

(defun count-frequencies (amt differences)
  (cl-loop for d in differences
           count (= d amt)))

(defun first-star (filename)
  (let* ((adapters (read-adapters filename))
         (built-in (+ (car (last adapters)) 3))
         (differences (jolt-differences (-snoc adapters built-in)))
         (ones (count-frequencies 1 differences))
         (threes (count-frequencies 3 differences)))
    (list ones threes (* ones threes))))

;; (count-frequencies 1 (jolt-differences (read-adapters "example.1")))
;; (jolt-differences (read-adapters "example.2"))

;; (first-star "example.1")
;; (first-star "example.2")
;; (first-star "input")
