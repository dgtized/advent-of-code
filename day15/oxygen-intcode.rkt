#lang racket

(require "../lib/intcode.rkt")

(define (key->dir key)
  (case key
    [(n) 1]
    [(s) 2]
    [(w) 3]
    [(e) 4]
    [else null]))

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

(define (world-coords world)
  (let ((xs (map tile-x world))
        (ys (map tile-y world)))
    (let ((xmin (apply min xs))
          (xmax (apply max xs))
          (ymin (apply min ys))
          (ymax (apply max ys)))
      ;; (list (+ 1 (abs xmin) (abs xmax)) (abs xmin)
      ;;       (+ 1 (abs ymin) (abs ymax)) (abs ymin))

      (list 40 10 50 40))))

(define (update-tile grid x y type)
  (vector-set! grid y
               (let ((copy (string-copy (vector-ref grid y))))
                 (string-set! copy x (case type
                                       [(wall) #\#]
                                       [(empty) #\.]
                                       [(oxygen) #\G]
                                       [(robot) #\D]))
                 copy)))

(define (print-world bot world)
  (match (world-coords world)
    [(list xsize xoffset ysize yoffset)
     (let ((grid (build-vector ysize (lambda (x) (make-string xsize #\ )))))
       (for [(piece world)]
         (match piece
           [(tile x y type)
            (update-tile grid
                         (+ x xoffset)
                         (+ y yoffset)
                         type)]))
       (match bot
         [(robot rx ry)
          (update-tile grid (+ rx xoffset) (+ ry yoffset) 'robot)])
       (for ([row (vector->list grid)])
         (println row)))]))

(define (repl cpu bot world)
  (print-world bot world)
  (println "Input: ")
  (let ((input (read)))
    (println (list input (key->dir input) bot))
    (if (eqv? input 'q)
        (list bot world)
        (if (null? (key->dir input))
            (repl cpu bot world) ;; no change
            (match (travel cpu bot (key->dir input))
              [(list ncpu nbot tile)
               (repl ncpu nbot (cons tile world))])))))

(let* ((program (load-program "input"))
       (bot (robot 0 0)))
  (repl (init-cpu program 0 1) bot (list (tile 0 0 'empty))))
