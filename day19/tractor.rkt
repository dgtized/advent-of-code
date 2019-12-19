#lang racket

(require racket/match)
(require "../lib/intcode.rkt")

(let* ((program (init-cpu (load-program "input") 0 1)))
  (foldl + 0
         (map
          (match-lambda
            [(list x y)
             (let ((ports (vector (list x y) '())))
               (run-until-blocked program ports)
               (car (vector-ref ports 1)))])
          (cartesian-product (range 0 50)
                             (range 0 50)))))
