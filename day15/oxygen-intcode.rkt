#lang racket

(require "../lib/intcode.rkt")

(define (key->dir key)
  (case key
    [(n) 1]
    [(s) 2]
    [(w) 3]
    [(e) 4]))

(struct robot (x y) #:transparent)
(struct tile (x y type) #:transparent)

(define (relative bot dir)
  (match bot
    [(robot x y)
     (case dir
       [(1) (robot x (- y 1))]
       [(2) (robot x (+ y 1))]
       [(3) (robot (- x 1) y)]
       [(4) (robot (+ x 1) y)])]))

(define (result bot dir code)
  (let ((new-bot (relative bot dir)))
    (match new-bot
      [(robot x y)
       (case code
         [(0) (list bot (tile x y 'wall))]
         [(1) (list new-bot (tile x y 'empty))]
         [(2) (list new-bot (tile x y 'oxygen))])])))

(define (travel cpu bot dir)
  (let* ((ports (vector (list dir) '()))
         (cpu (run-until-blocked cpu ports))
         (code (car (vector-ref ports 1))))
    (cons cpu (result bot dir code))))

(define (repl cpu bot world)
  (println (list bot world))
  (println "Input: ")
  (let ((input (read)))
    (println (list input (key->dir input)))
    (if (eqv? input 'q)
        (list bot world)
        (match (travel cpu bot (key->dir input))
          [(list ncpu nbot tile)
           (repl ncpu nbot (cons tile world))]))))

(let* ((program (load-program "input"))
       (bot (robot 0 0)))
  (repl (init-cpu program 0 1) bot '()))
