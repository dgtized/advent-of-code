#lang racket

(define (solve-next lst)
  (let ((previous (index-of (rest lst) (first lst))))
    (if previous
        (cons (+ previous 1) lst)
        (cons 0 lst))))

(define (solve-up-to lst iter n)
  (if (= iter n)
      lst
      (solve-up-to (solve-next lst) (+ 1 iter) n)))

(define (solve-to-n lst n)
  (solve-up-to lst (length lst) n))

(module+ test
  (require rackunit)
  (check-equal? (solve-to-n (reverse '(0 3 6)) 10)
                (reverse '(0 3 6 0 3 3 1 0 4 0)))
  (check-equal? (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 2020))
                203))

