#lang racket

(require racket/string)

(define (partition-into lst n)
  (if (empty? lst)
      '()
      (cons (take lst n) (partition-into (drop lst n) n))))

(define (column lst j)
  (for/list ([i (range 5)])
    (list-ref lst (+ j (* 5 i)))))

(define (board->rows board)
  (partition-into board 5))

(define (board->columns board)
  (for/list ([j (range 5)])
    (column board j)))

(define (parse-bingo-boards input)
  (for/list [(board (partition-into (map string->number (string-split input #:trim? #t)) 25))]
    board))

(define (load-input filename)
  (let [(lines (file->lines filename))]
    (list (map string->number (string-split (first lines) ","))
          (parse-bingo-boards (string-join (rest (rest lines)))))))

(define (bingo? lst sequence)
  (andmap (lambda (x) (member x sequence)) lst))

;; (bingo? '(1 2 3 4 5) '(1 2 3 4 5 6))
;; (bingo? '(1 2 3 4 5) '(5))

(define (winning-board? board sequence)
  (ormap (lambda (s) (bingo? s sequence))
         (append (board->rows board)
                 (board->columns board))))

(define (winning-sequence board sequence)
  (for/or ((upto (range (length sequence))))
    (let ((sequence-thus-far (take sequence upto)))
      (if (winning-board? board sequence-thus-far)
          sequence-thus-far
          #f))))

(define (winning-boards input)
  (let ((sequence (first input))
        (boards (cadr input)))
    (sort (for/list ((board (in-list boards)))
            (list board (winning-sequence board sequence)))
                 #:key (lambda (x) (length (cadr x)))
                 <)))

(define (score-board board-seq)
  (let ((board (car board-seq))
        (winning-seq (cadr board-seq)))
    (apply * (list (foldl + 0 (filter (lambda (x) (not (member x winning-seq))) board))
                   (last winning-seq)))))

(require rackunit)

(let ((example (winning-boards (load-input "example"))))
  (check-equal? (score-board (first example)) 4512)
  (check-equal? (score-board (last example)) 1924))

(let ((input (winning-boards (load-input "input"))))
  (check-equal? (score-board (first input)) 35670)
  (check-equal? (score-board (last input)) 22704))
