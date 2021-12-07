#lang racket

(require racket/string)
(require rackunit)

(define (string->input s)
  (map string->number (string-split s ",")))

(define (file->input filename)
  (string->input (car (file->lines filename))))

(define (cost positions n)
  (apply + (map (lambda (x) (let ((d (abs (- n x))))
                              (/ (* d (+ d 1)) 2)))
                positions)))

(check-equal? (cost (file->input "example") 2) 206)
(check-equal? (cost (file->input "example") 5) 168)

(define (min-cost positions)
  (argmin (lambda (x) (cost positions x))
          (range (argmin (lambda (x) x) positions)
                 (+ 1 (argmax (lambda (x) x) positions)))))

(check-equal? (min-cost (file->input "example")) 5)

(check-equal?
 (let* ((positions (file->input "input"))
        (solution (min-cost positions)))
   (list solution (cost positions solution)))
 '(483 102245489))



