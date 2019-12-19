#lang racket

(require "../lib/intcode.rkt")

(let ((program (init-cpu (load-program "input") 0 1)))
  (define (pulling? args)
    (let ((ports (vector args '())))
      (run-until-blocked program ports)
      (car (vector-ref ports 1))))
  (foldl + 0
         (map pulling?
              (cartesian-product (range 0 50)
                                 (range 0 50)))))


