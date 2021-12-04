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

(load-input "example")
