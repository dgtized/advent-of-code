#lang racket

(require "../lib/intcode.rkt")

(define (split-groups lst n)
  (if (empty? lst)
      '()
      (cons (take lst n) (split-groups (drop lst n) n))))

(define (print-grid output)
  (let ((grid (build-vector 22 (lambda (x) (make-string 38 #\ )))))
    (for [(coords output)]
      (match coords
        [(list x y color)
         (if (= x -1)
             null
             (vector-set! grid y
                          (let ((copy (string-copy (vector-ref grid y))))
                            (string-set! copy x (case color
                                                  [(0) #\ ]
                                                  [(1) #\W]
                                                  [(2) #\*]
                                                  [(3) #\P]
                                                  [(4) #\B]))
                            copy)))]))
    (for ([row (vector->list grid)])
      (println row))
    (println (length (filter (lambda (c) (eq? c #\*))
                             (flatten (map string->list (vector->list grid))))))))

;; paint
(let* ((ports (list->vector '(() ())))
       (cpu (run-until-blocked (init-cpu (load-program "input") 0 1)
                               ports))
       (output (split-groups (vector-ref ports 1) 3)))
  (print-grid output))

(define (find-score output)
  (let ((hit (findf (lambda (coords) (and (= (car coords) -1)
                                          (= (cadr coords) 0)))
                    output)))
    (if hit
        (third hit)
        hit)))

(define (find-block-x block output)
  (let ((hit (findf (lambda (coords) (= (third coords) block))
                    output)))
    (if hit
        (car hit)
        0)))

(define (run-game cpu ports score)
  (let ((cpu (run-until-blocked cpu ports))
        (output (split-groups (vector-ref ports 1) 3)))
    (if (eq? 'halt (cpu-condition cpu))
        score
        (let* ((new-score (find-score output))
               (ball-x (find-block-x 4 output))
               (paddle-x (find-block-x 3 output))
               (command (cond ((< ball-x paddle-x) -1)
                              ((> ball-x paddle-x) 1)
                              (else 0))))
          (println (list ball-x paddle-x command))
          (run-game cpu (vector (list command) '())
                    (if (and new-score (> new-score score)) new-score score))))))

(let* ((ports (vector '() '()))
       (program (load-program "input")))
  (vector-set! program 0 2) ;; infinite plays
  (run-game (init-cpu program 0 1) ports 0))



