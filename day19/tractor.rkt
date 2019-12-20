#lang racket

(require "../lib/intcode.rkt")

(let ((program (init-cpu (load-program "input") 0 1)))
  (define (tractor-pull args)
    (let ((ports (vector args '())))
      (run-until-blocked program ports)
      (car (vector-ref ports 1))))
  (println
   (foldl + 0
          (map tractor-pull
               (cartesian-product (range 0 50)
                                  (range 0 50)))))

  (define (h-scan y)
    (let ((scan (remove null
                        (filter (lambda (x) (> (tractor-pull (list x y)) 0))
                                (range 0 2000)))))
      (list (first scan) (last scan))))

  (define (v-scan x)
    (let ((scan (remove null
                        (filter (lambda (y) (> (tractor-pull (list x y)) 0))
                                (range 0 2000)))))
      (list (first scan) (last scan))))

  ;; candidates
  ;; (println (map (lambda (x) (list x (h-scan x) (v-scan x))) (range 200 1000 100)))
  '((200 (228 268) (150 175))
    (300 (342 402) (224 263))
    (400 (456 536) (299 351))
    (500 (570 670) (373 438))
    (600 (684 804) (448 526))
    (700 (798 938) (522 614))
    (800 (912 1073) (597 702))
    (900 (1026 1207) (671 790)))

  (define (pulling? x y)
    (let ((res (tractor-pull (list x y))))
      (println (list x y res))
      (= 1 res)))

  (define (satisfies? x y)
    (and (pulling? x y)
         (pulling? (+ x 100) y)
         (pulling? x (+ y 100))
         (pulling? (+ x 100) (+ y 100))))

  (println (satisfies? 80 1000)))

