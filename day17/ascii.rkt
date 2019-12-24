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

(let* ((program (load-program "input"))
       (ports (vector '() '())))
  (run-until-blocked (init-cpu program 0 1) ports)
  (display (view-cam (vector-ref ports 1))))
