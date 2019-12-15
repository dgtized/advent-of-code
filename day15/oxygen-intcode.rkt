#lang racket

(require "../lib/intcode.rkt")

(let* ((ports (vector '() '()))
       (program (load-program "input")))
  (run-until-blocked (init-cpu program 0 1) ports)
  ports)



