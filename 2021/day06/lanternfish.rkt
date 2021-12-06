#lang racket

(require racket/string)
(require rackunit)

(define (string->input s)
  (map string->number (string-split s ",")))

(define (file->input filename)
  (string->input (car (file->lines filename))))

;; (file->input "input")
(check-equal? '(3 4 3 1 2) (file->input "example"))

(define (tick lst)
  (let ((fresh (length (filter (lambda (x) (= x 0)) lst))))
    (append (map (lambda (x)
                   (if (= x 0) 6 (- x 1)))
                 lst)
            (make-list fresh 8))))

(check-equal? '(2 3 2 0 1) (tick '(3 4 3 1 2)))
(check-equal? '(1 2 1 6 0 8) (tick '(2 3 2 0 1)))
(check-equal? '(0 1 0 5 6 7 8) (tick '(1 2 1 6 0 8)))
(check-equal? '(6 0 6 4 5 6 7 8 8) (tick '(0 1 0 5 6 7 8)))
(check-equal?
 (string->input "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8")
 (tick (string->input "0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8")))

(define (simulate input days)
  (if (= days 0)
      input
      (simulate (tick input) (- days 1))))

(check-equal? (string->input "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8")
              (simulate '(3 4 3 1 2) 18))

(check-equal? 26 (length (simulate '(3 4 3 1 2) 18)))

(check-equal? 5934 (length (simulate (file->input "example") 80)))
(check-equal? 355386 (length (simulate (file->input "input") 80)))

(define (frequencies input)
  (let ((base (make-vector 9 0)))
    (map (lambda (x) (let ((v (vector-ref base x)))
                       (vector-set! base x (+ v 1))))
         input)
    base))

(check-equal? '#(0 1 1 2 1 0 0 0 0) (frequencies '(3 4 3 1 2)))

(define (vec-tick base)
  (let ((next (make-vector 9 0)))
    (vector-set! next 0 (vector-ref base 1))
    (vector-set! next 1 (vector-ref base 2))
    (vector-set! next 2 (vector-ref base 3))
    (vector-set! next 3 (vector-ref base 4))
    (vector-set! next 4 (vector-ref base 5))
    (vector-set! next 5 (vector-ref base 6))
    (vector-set! next 6 (+ (vector-ref base 7) (vector-ref base 0)))
    (vector-set! next 7 (vector-ref base 8))
    (vector-set! next 8 (vector-ref base 0))
    next))

(check-equal? (vec-tick (frequencies '(3 4 3 1 2))) (frequencies '(2 3 2 0 1)))
(check-equal?
 (vec-tick (frequencies (string->input "0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8")))
 (frequencies (string->input "6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8")))

(define (sim-vec input days)
  (if (= days 0)
      (apply + (vector->list input))
      (sim-vec (vec-tick input) (- days 1))))

(check-equal? (sim-vec (frequencies (file->input "example")) 256)
              26984457539)

(check-equal? (sim-vec (frequencies (file->input "input")) 256)
              1613415325809)

;; (time (length (simulate (file->input "input") 256)))


