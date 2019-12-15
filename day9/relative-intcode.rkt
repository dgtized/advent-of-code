#lang racket

(require "../lib/intcode.rkt")

(define (run-source source)
  (let ((memory (parse-program source))
        (ports (list->vector '(() ()))))
    (run-until-blocked (cpu memory 0 0 0 1 'run) ports)
    ports))

(define (port-output ports)
  (vector-ref ports 1))

;; quine
(let* ((program "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
       (out (port-output (run-source program))))
  (list out (equal? program (string-join (map number->string out) ","))))
(let ((out (car (port-output (run-source "1102,34915192,34915192,7,4,7,99,0")))))
  (list out (= 16 (string-length (number->string out)))))
(let ((out (car (port-output (run-source "104,1125899906842624,99")))))
  (list out (= 1125899906842624 out)))

;; (set! debugging #t)
(let* ((memory (load-program "input"))
      (ports (list->vector '((1) ())))
      (cpu (run-until-blocked (cpu memory 0 0 0 1 'run) ports)))
  (list ports))

(let* ((memory (load-program "input"))
      (ports (list->vector '((2) ())))
      (cpu (run-until-blocked (cpu memory 0 0 0 1 'run) ports)))
  (list ports))
