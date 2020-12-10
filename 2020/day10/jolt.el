(require 's)
(require 'f)
(require 'dash)

(defun read-adapters (filename)
  "Read all the lines from `filename' into a list, convert to
numbers and sort ascending."
  (sort (mapcar #'string-to-number
                (s-lines (s-trim (f-read-text filename))))
        #'<))

(defun jolt-differences (adapters)
  "Find the jolt difference between each `adapter'.

Note that 0 is the joltage of source, and built-in adapter is 3
more than last adapter used."
  (let ((built-in (+ (car (last adapters)) 3)))
    (cl-loop for a in (cons 0 adapters)
             for b in (-snoc adapters built-in)
             collect (- b a))))

(defun count-frequencies (value differences)
  "Number of times `value' occurs in `differences'."
  (cl-loop for x in differences
           count (= x value)))

(defun first-star (filename)
  "Count 1s and 3s from differences in adapters, and multiply"
  (let* ((adapters (read-adapters filename))
         (differences (jolt-differences adapters))
         (ones (count-frequencies 1 differences))
         (threes (count-frequencies 3 differences)))
    (list ones threes (* ones threes))))

;; (count-frequencies 1 (jolt-differences (read-adapters "example.1")))
;; (jolt-differences (read-adapters "example.2"))

;; (first-star "example.1") ;; => (7 5 35)
;; (first-star "example.2") ;; => (22 10 220)
;; (first-star "input") ;; => (71 31 2201)

(defun cases (n)
  "Permutation count for a sequence of 1s of length `n'"
  (cl-case n
    (1 1)
    (2 2)
    (3 4)
    (4 7)))

(defun second-star (filename)
  "Calculate number of permutations of viable adapter joltage sequences"
  (let* ((adapters (read-adapters filename))
         ;; Remove any differences of size 3 as it's impossible to remove those gaps
         (sets (-split-on 3 (jolt-differences adapters)))
         ;; Each group is a set of consecutive 1s, so we lookup for a group of
         ;; that size how many permutations are possible.
         (permutations (cl-loop for group in sets
                                collect (cases (length group)))))
    ;; Calculate the product of each group with permutations
    (-reduce '* permutations)))

;; (second-star "example.1") ;; 8
;; (second-star "example.2") ;; 19208
;; (second-star "input") ;; 169255295254528
