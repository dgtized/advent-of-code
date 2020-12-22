#lang racket

(define (parse-decks filename)
  (let ((players (regexp-split #rx"\n\n" (file->string filename))))
    (map (lambda (deck)
           (map string->number (rest (string-split deck "\n"))))
         players)))

(define (round decks)
  (match-let (((list p1 p2) decks))
    (let [(c1 (first p1))
          (c2 (first p2))]
      (if (>= c1 c2)
          (list (append (rest p1) (list c1 c2)) (rest p2))
          (list (rest p1) (append (rest p2) (list c2 c1)))))))

(define (run-game decks)
  (match-let (((list p1 p2) decks))
    (if (or (empty? p1) (empty? p2))
        decks
        (run-game (round decks)))))

(define (score decks)
  (let ((winner (flatten decks)))
    (for/sum ([i (range 1 (+ 1 (length winner)))]
              [v (reverse winner)])
      (* i v))))

;; (= 306 (score (run-game (parse-decks "example"))))
;; (= 32472 (score (run-game (parse-decks "input"))))
