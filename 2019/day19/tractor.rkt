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


  (define (pulling? x y)
    (let ((res (tractor-pull (list x y))))
      (= 1 res)))

  (define (satisfies? x y)
    (and (pulling? x y)
         (pulling? (+ x 99) y)
         (pulling? x (+ y 99))
         (pulling? (+ x 99) (+ y 99))))

  ;; candidates
  (for [(y (range 1050 1070 1))]
    (match-let* ([(list xmin xmax) (h-scan y)])
      (let ((cx (- xmax 100)))
        (match-let ([(list ymin ymax) (v-scan cx)])
          (let ((cy (- ymax 100)))
            (when (and (> cx xmin)
                       (> cy ymin)
                       (satisfies? cx cy))
              (println (list y cx cy (+ (* 10000 cx) cy)
                             (list 'x xmin xmax) (list 'y ymin ymax)
                             ))))))))

  (for*/first [(x (range 1250 1400 1))
               (y (range 1000 1100 1))
               #:when (satisfies? x y)]
    (list x y (+ (* 10000 x) y))))

