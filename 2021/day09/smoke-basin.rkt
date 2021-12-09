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

(define (basins input)
  (let [(height (vector-length input))
        (width (vector-length (vector-ref input 0)))]
    (for*/fold ((acc '()))
               ((y (range height))
                (x (range width)))
      (let [(value (lookup input (cons x y)))]
        (cond ((= value 9)
               (values acc))
              ((for/first ((v (map (lambda (c) (lookup input c))
                                   (filter (lambda (coord) (valid? width height coord))
                                           (neighbors (cons x y)))))
                           #:when (<= v value))
                 v)
               (values acc))
              (else
               (values (cons (cons x y) acc))))))))

(define (grow input basin)
  (let* [(height (vector-length input))
         (width (vector-length (vector-ref input 0)))
         (expansions (for/list ((coord (in-set basin)))
                       (list->set (filter (lambda (c) (and (valid? width height c)
                                                           (< (lookup input c) 9)))
                                          (neighbors coord)))))
         (basin2 (apply set-union (cons basin expansions)))]
    (if (proper-subset? basin basin2)
        (grow input basin2)
        basin)))

(define (part2 input)
  (take (sort (for/list ((basin (basins input)))
                (set-count (grow input (set basin))))
              >)
        3))

(check-equal? 1134 (apply * (part2 (load-input "example"))))
(check-equal? 899392 (apply * (part2 (load-input "input"))))


