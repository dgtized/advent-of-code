#lang racket

(require racket/string)

(define (load-input filename)
  (let [(lines (file->lines filename))]
    (list->vector (map (lambda (x)
                         (list->vector (map (lambda (c) (- (char->integer c)
                                                           (char->integer #\0)))
                                            (string->list x))))
                       lines))))

(define (valid? width height coords)
  (let [(x (car coords))
        (y (cdr coords))]
    (and (>= x 0) (< x width)
         (>= y 0) (< y height))))

(define (neighbors coords)
  (let [(x (car coords))
        (y (cdr coords))]
    (list (cons x (+ y 1))
          (cons x (- y 1))
          (cons (+ x 1) y)
          (cons (- x 1) y))))

(define (lookup input coords)
  (let [(x (car coords))
        (y (cdr coords))]
    (vector-ref (vector-ref input y) x)))

(define (first-star input)
  (let [(height (vector-length input))
        (width (vector-length (vector-ref input 0)))]
    (for*/sum ((y (range height))
               (x (range width)))
      (let [(value (lookup input (cons x y)))]
        (if (not (for/first ((v (map (lambda (c) (lookup input c))
                                     (filter (lambda (coord) (valid? width height coord))
                                      (neighbors (cons x y)))))
                             #:when (<= v value))
                   v))
            (+ 1 value)
            0)))))

(require rackunit)

(check-equal? 570 (first-star (load-input "input")))
(check-equal? 15 (first-star (load-input "example")))
