#lang racket

(define (parse-input filename)
  (let [(lines (file->lines filename))]
    (list->vector (map (lambda (x)
                         (list->vector (map (lambda (c) (- (char->integer c)
                                                           (char->integer #\0)))
                                            (string->list x))))
                       lines))))

(define (neighbors coords)
  (match-let ([(cons x y) coords])
    (for*/list [(i '(-1 0 1))
                (j '(-1 0 1))
                #:when (not (= i j 0))]
      (cons (+ x i) (+ y j)))))

(define (legal? coord)
  (match-let ([(cons x y) coord])
    (and (>= x 0) (< x 10)
         (>= y 0) (< y 10))))

(parse-input "example")
;; (neighbors (cons 1 1))

(define (incr-step grid)
  (for/vector [(line (in-vector grid))]
    (build-vector 10 (lambda (i) (+ 1 (vector-ref line i))))))

(define (to-flash grid)
  (filter pair?
          (for*/list [(i (range 10))
                      (j (range 10))]
            (let [(v (vector-ref (vector-ref grid j) i))]
              (when (>= v 9)
                (cons i j))))))

(define (update-grid! grid coord v)
  (match-let ([(cons x y) coord])
    (let ((line (vector-ref grid y)))
      (vector-set! line x 0)
      (vector-set! grid y line))))

(define (flash grid coord)
  (set! flashes (+ 1 flashes))
  (match-let ([(cons x y) coord])
    (let ((v (vector-ref (vector-ref grid y) x)))
      (cond ((= v 0) '())
            ((> v 9)
             (begin (update-grid! grid coord 0)
                    (filter legal? (neighbors coord))))
            (else (update-grid! grid coord (+ v 1))
                  (if (> (+ v 1) 9)
                      (list coord)
                      '()))))))

(define (flash-step grid coords)
  (let ((pass (apply set-union (filter (map (lambda (c) (list->set (flash grid c))) coords)))))
    (if (empty? pass)
        grid
        (flash-step grid (set->list pass)))))

(define (step grid n)
  (if (zero? n)
      grid
      (let ((grid (incr-step grid)))
        (step (flash-step grid (to-flash grid)) (- n 1)))))

(define flashes 0)
(step (parse-input "example") 100)
flashes

