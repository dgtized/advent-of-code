#lang racket

(define (solve-next lst iter seen)
  (let ((previous (hash-ref seen (first lst) #f)))
    (if previous
        (- iter previous)
        0)))

(define (solve-up-to lst seen iter n)
  (if (= iter n)
      lst
      (let ((next (solve-next lst iter seen)))
        (solve-up-to (cons next lst)
                     (hash-set seen (car lst) iter)
                     (+ 1 iter) n))))

(define (solve-to-n lst n)
  (let ((seen (apply hash-set*
                     (cons (make-immutable-hash)
                           (flatten
                            (for/list ([i (range 1 (length lst))]
                                       [elem (reverse (rest lst))])
                              (cons elem i)))))))
    (solve-up-to lst
                 seen
                 (length lst) n)))

(module+ test
  (require rackunit)
  (check-equal? (reverse (solve-to-n (reverse '(0 3 6)) 10))
                '(0 3 6 0 3 3 1 0 4 0))
  (check-equal? (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 2020))
                203)

  ;; These take 54s or so to run each, so commented out
  ;; Verify Example
  ;; (time (check-equal? (car (solve-to-n (reverse '(0 3 6)) 30000000))
  ;;                     175594))
  ;; Solution

  ;; (time (check-equal? (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 30000000))
  ;;                     9007186))
  )
