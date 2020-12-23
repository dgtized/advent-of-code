#lang racket

(define (string->cups s)
  (map (lambda (x) (string->number (string x))) (string->list s)))

(define (find-destination current remaining-cups)
  (let ((pos (index-of remaining-cups current)))
    (cond ((number? pos) pos)
          ((< current 1)
           (find-destination 9 remaining-cups))
          (else (find-destination (- current 1) remaining-cups)))))

(define (crab-cup current cups)
  (let* ((current-pos (index-of cups current))
         (head (take cups (+ 1 current-pos)))
         (tail (drop cups (+ 1 current-pos)))
         (pickup (take (append tail head) 3))
         (remaining-cups (remq* pickup cups))
         (destination (find-destination (- current 1) remaining-cups)))
    (append (take remaining-cups (+ 1 destination))
            pickup
            (drop remaining-cups (+ 1 destination)))
    ))

(define (run-game current cups iters)
  (if (= iters 0)
      cups
      (let* ((next (crab-cup current cups))
             (current-pos (index-of next current)))
        (run-game (list-ref next (if (= current-pos 8) 0 (+ 1 current-pos)))
                  next
                  (- iters 1)))))

(define (first-star cups)
  (let ((pos (index-of cups 1)))
    (string-join (map number->string (append (drop cups (+ 1 pos)) (take cups pos))) "")))

(module+ test
  (require rackunit)

  (check-equal? 1 (find-destination 2 '(3 2 5 4 6 7)))
  (check-equal? '(3 2 8 9 1 5 4 6 7) (crab-cup 3 (string->cups "389125467")))
  (check-equal? '(3 2 5 4 6 7 8 9 1) (crab-cup 2 (string->cups "328915467")))
  (check-equal? '(3 4 6 7 2 5 8 9 1) (crab-cup 5 (string->cups "325467891")))
  (check-equal? '(5 8 3 7 4 1 9 2 6) (run-game 3 (string->cups "389125467") 10))
  (check-equal? "92658374" (first-star '(5 8 3 7 4 1 9 2 6)))
  (check-equal? "67384529" (first-star (run-game 3 (string->cups "389125467") 100)))
  (check-equal? "43769582" (first-star (run-game 4 (string->cups "467528193") 100)))
  )

