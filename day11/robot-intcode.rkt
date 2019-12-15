#lang racket

(require "../lib/intcode.rkt")

(struct robot (x y cardinal))

(define/match (turn cardinal rotate)
  [('north 0) 'west]
  [('north 1) 'east]
  [('east 0) 'north]
  [('east 1) 'south]
  [('south 0) 'east]
  [('south 1) 'west]
  [('west 0) 'south]
  [('west 1) 'north])

(define (advance bot rotate)
  (match bot
    [(robot x y cardinal)
     (let ((new-cardinal (turn cardinal rotate)))
       (case new-cardinal
         [(north) (struct-copy robot bot
                                [y (+ y 1)] [cardinal new-cardinal])]
         [(east) (struct-copy robot bot
                                [x (+ x 1)] [cardinal new-cardinal])]
         [(south) (struct-copy robot bot
                                [y (- y 1)] [cardinal new-cardinal])]
         [(west) (struct-copy robot bot
                              [x (- x 1)] [cardinal new-cardinal])]))]))

(struct panel (x y color))

(define (matching-pane px py)
  (lambda (pane)
    (match pane
        [(panel x y _) (and (= px x) (= py y))])))

(define (run-robot cpu ports bot panels)
  (match-let* ([(robot px py _) bot]
               [panel-at-pos (findf (matching-pane px py) panels)]
               [pcolor (if panel-at-pos (panel-color panel-at-pos) 0)])
    ;; (println (list"@ " px py pcolor))
    (write-port! ports 0 pcolor)
    (let* ((cpu (run-until-blocked cpu ports))
           (output (vector-ref ports 1))
           (new-bot (advance bot (cadr output)))
           (new-panel (panel (robot-x bot) (robot-y bot) (car output))))
      (if (equal? 'halt (cpu-condition cpu))
          (reverse panels)
          (run-robot cpu (list->vector (list '() '()))
                     new-bot
                     (cons new-panel panels))))))

(let ((ports (list->vector '(() ())))
      (cpu (init-cpu (load-program "input") 0 1))
      (bot (robot 0 0 'north))
      (path (list (panel 0 0 0))))
  (set-count (list->set (map (lambda (pane) (match pane [(panel x y _) (list x y)]))
                             (run-robot cpu ports bot path)))))

;; paint
(let* ((ports (list->vector '(() ())))
       (cpu (init-cpu (load-program "input") 0 1))
       (bot (robot 50 50 'north))
       (path (run-robot cpu ports bot (list (panel 50 50 1))))
       (grid (build-vector 100 (lambda (x) (make-string 100 #\ )))))
  (for ([pane path])
    (match pane
      [(panel x y color)
       (vector-set! grid y (let ((copy (string-copy (vector-ref grid y))))
                             (string-set! copy x (if (= color 0) #\ #\+))
                             copy))]))
  (for ([row (reverse (vector->list grid))])
    (println row)))


