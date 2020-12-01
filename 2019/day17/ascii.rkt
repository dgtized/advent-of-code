#lang racket

(require "../lib/intcode.rkt")

(define (is-10? x) (= 10 x))
(define (is-not-10? x) (not (= 10 x)))

(define (view-cam lst)
  (if (empty? lst)
      ""
      (let ((row (takef lst is-not-10?)))
        (string-append (list->string (map integer->char row)) "\n"
                       (view-cam (drop lst (+ (length row) 1)))))))

(define (surroundings idx len)
  (filter (lambda (x) (and (>= x 0) (< x len)))
          (list idx (- idx 38) (- idx 1) (+ idx 1) (+ idx 38))))

(define (intersection? idx grid)
  (let ((adjacent (map (lambda (x) (vector-ref grid x))
                       (surroundings idx (vector-length grid)))))
    (apply = 35 adjacent)))

(define *grid* (vector 0))
(let* ((program (load-program "input"))
       (ports (vector '() '())))
  (run-until-blocked (init-cpu program 0 1) ports)
  (let* ((output (vector-ref ports 1))
         (grid (list->vector output)))
    (for ((x (surroundings (+ 38 1) (vector-length grid))))
      (vector-set! grid x (char->integer #\O)))
    (display (view-cam (vector->list grid)))
    (set! *grid* grid)
    (for/sum [(idx (in-range (vector-length grid)))
              #:when (intersection? idx grid)]
      (* (quotient idx 38) (modulo idx 38)))))
