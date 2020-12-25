#lang racket

(define (transform-loop subject value loop-size)
  (if (= loop-size 0)
      value
      (transform-loop subject
                      (remainder (* value subject) 20201227)
                      (- loop-size 1))))

(define (find-loop-for-key public-key iter subject value)
  (if (= public-key value)
      iter
      (find-loop-for-key
       public-key
       (+ 1 iter)
       subject
       (remainder (* value subject) 20201227))))

(define (encryption-key card-key door-key)
  (transform-loop door-key 1 (find-loop-for-key card-key 0 7 1)))

(module+ test
  (require rackunit)

  (check-equal? 14897079 (encryption-key 5764801 17807724))
  (check-equal? 42668 (encryption-key 2084668 3704642)))
