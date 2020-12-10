(require 's)
(require 'f)
(require 'dash)

(defun read-adapters (filename)
  (sort (mapcar #'string-to-number
                (s-lines (s-trim (f-read-text filename))))
        #'<))

(defun jolt-differences (adapters)
  (let ((built-in (+ (car (last adapters)) 3)))
    (cl-loop for a in (cons 0 adapters)
             for b in (-snoc adapters built-in)
             collect (- b a))))

(defun count-frequencies (amt differences)
  (cl-loop for d in differences
           count (= d amt)))

(defun first-star (filename)
  (let* ((adapters (read-adapters filename))
         (differences (jolt-differences adapters))
         (ones (count-frequencies 1 differences))
         (twos (count-frequencies 2 differences))
         (threes (count-frequencies 3 differences)))
    (list ones twos threes (* ones threes))))

;; (count-frequencies 1 (jolt-differences (read-adapters "example.1")))
;; (jolt-differences (read-adapters "example.2"))

;; (first-star "example.1")
;; (first-star "example.2")
;; (first-star "input")
