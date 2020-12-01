#lang racket

(require "../lib/intcode.rkt")

(define (is-not-10? x) (not (= 10 x)))
(define (view-cam lst)
  (if (empty? lst)
      ""
      (let ((row (takef lst is-not-10?)))
        (string-append (list->string (map integer->char row)) "\n"
                       (view-cam (drop lst (+ (length row) 1)))))))

(define (read-input)
  (map char->integer (string->list (string-append (read-line) "\n"))))

(define (repl cpu ports)
  (run-until-blocked cpu ports)
  (display (view-cam (vector-ref ports 1)))
  (repl cpu (vector (read-input) '())))

(repl (init-cpu (load-program "input") 0 1)
      (vector '() '()))

