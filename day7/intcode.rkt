#lang racket

(require "../lib/intcode.rkt")

(define (run-sequence memory input output ports)
  (let ((state (run-until-blocked (init-cpu memory input output) ports)))
    (read-port! ports 5)))

(define (thruster-seq memory input)
  (let* ((ports (vector (list (list-ref input 0) 0)
                        (list (list-ref input 1))
                        (list (list-ref input 2))
                        (list (list-ref input 3))
                        (list (list-ref input 4))
                        (list)))
         (a (run-sequence memory 0 1 ports))
         (b (run-sequence memory 1 2 ports))
         (c (run-sequence memory 2 3 ports))
         (d (run-sequence memory 3 4 ports))
         (e (run-sequence memory 4 5 ports)))
    e))

;; (thruster-seq (load-program "input.43210") '(4 3 2 1 0))

(let ((memory (load-program "input.43210")))
  (thruster-seq memory (argmax (lambda (s) (thruster-seq memory s)) (permutations '(0 1 2 3 4)))))

;; (let ((memory (load-program "input.54321")))
;;   (thruster-seq memory (argmax (lambda (s) (thruster-seq memory s)) (permutations '(0 1 2 3 4)))))

(let ((memory (load-program "input")))
  (thruster-seq memory (argmax (lambda (s) (thruster-seq memory s)) (permutations '(0 1 2 3 4)))))

;; (let ((memory (load-program "input.54321")))
;;   (argmax (lambda (s) (thruster-seq memory s)) (permutations '(0 1 2 3 4))))

(define (step-all machines ports)
  (map (lambda (m) (run-until-blocked m ports)) machines))

(define (any-running? machines)
  (ormap (lambda (m) (eq? 'run (cpu-condition m)))
          machines))

(define (run-all machines ports)
  (if (any-running? machines)
      (run-all (step-all machines ports) ports)
      machines))

(define (amplifiers memory input)
  (let* ((ports (vector (list (list-ref input 0) 0)
                        (list (list-ref input 1))
                        (list (list-ref input 2))
                        (list (list-ref input 3))
                        (list (list-ref input 4))
                        (list)))
         (machines (list (init-cpu memory 0 1)
                         (init-cpu memory 1 2)
                         (init-cpu memory 2 3)
                         (init-cpu memory 3 4)
                         (init-cpu memory 4 0))))
    (run-all machines ports)
    ports))

(let ((program (load-program "input")))
  (let ((best (argmax (lambda (s) (read-port! (amplifiers program s) 0)) (permutations '(5 6 7 8 9)))))
    (amplifiers program best)))

;; (for ([input (permutations '(5 6 7 8 9))])
;;   (println (list input
;;                  (read-port! (amplifiers (load-program "input") input) 0))))
