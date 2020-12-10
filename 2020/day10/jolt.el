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
         (threes (count-frequencies 3 differences)))
    (list ones threes (* ones threes))))

;; (count-frequencies 1 (jolt-differences (read-adapters "example.1")))
;; (jolt-differences (read-adapters "example.2"))

;; (first-star "example.1") ;; => (7 5 35)
;; (first-star "example.2") ;; => (22 10 220)
;; (first-star "input") ;; => (71 31 2201)

(defun cases (n)
  (cl-case n
    (1 1)
    (2 2)
    (3 4)
    (4 7)))

(defun second-star (filename)
  (let* ((adapters (read-adapters filename))
         (sets (-split-on 3 (jolt-differences adapters)))
         (permutations (cl-loop for group in sets
                                collect (cases (length group)))))
    (-reduce '* permutations)))

;; (second-star "example.1") ;; 8
;; (second-star "example.2") ;; 19208
;; (second-star "input") ;; 169255295254528
