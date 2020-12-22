#lang racket

(require racket/set)

(define (parse-decks filename)
  (let ((players (regexp-split #rx"\n\n" (file->string filename))))
    (map (lambda (deck)
           (map string->number (rest (string-split deck "\n"))))
         players)))

(define (normal-round decks)
  (match-let (((list p1 p2) decks))
    (let [(c1 (first p1))
          (c2 (first p2))]
      (if (>= c1 c2)
          (list (append (rest p1) (list c1 c2)) (rest p2))
          (list (rest p1) (append (rest p2) (list c2 c1)))))))

(define (run-game decks)
  (if (ormap empty? decks)
      decks
      (run-game (normal-round decks))))

(define (score decks)
  (let ((winner (flatten decks)))
    (for/sum ([i (range 1 (+ 1 (length winner)))]
              [v (reverse winner)])
      (* i v))))

(define (hash-decks decks)
  (for/list ([deck decks])
    (for/sum ([i (range 1 (+ 1 (length deck)))]
                 [v (reverse deck)])
      (* i v))))

(define (recursive-round decks seen)
  (match-let* (((list p1 p2) decks)
               ((list* c1 r1) p1)
               ((list* c2 r2) p2))
    ;; (println (list "recursion" (set-count seen) p1 p2))
    (cond ((and (<= c1 (length r1)) (<= c2 (length r2)))
           ;; (println (list "Starting recursive-game" (set-count seen)))
           (match-let (((list s1 s2)
                        (recursive-game (list (take r1 c1)
                                              (take r2 c2))
                                        (list->set '()))))
             ;; (println (list "Exiting recursive-game" (set-count seen)))
             (cond ((empty? s2)
                    (list (append r1 (list c1 c2)) r2))
                   ((empty? s1)
                    (list r1 (append r2 (list c2 c1))))
                   (else (list (append r1 (list c1 c2)) r2)))))
          (else (normal-round decks)))))

(define (recursive-game decks seen)
  (let ((deck-hash (hash-decks decks)))
    (cond ((or (ormap empty? decks))
           ;; (println (list "win by empty" decks))
           decks)
          ((set-member? seen deck-hash)
           ;; (println (list "player 1 wins by previous" decks))
           decks)
          (else
           (let ((new-deck (recursive-round decks seen)))
             (recursive-game new-deck (set-add seen deck-hash)))))))

(module+ test
  (require rackunit)
  (check-equal? '(4 0) (hash-decks '((1 2) ())))
  (check-equal? '(0 4) (hash-decks '(() (1 2))))
  (check-equal? 306 (score (run-game (parse-decks "example"))))
  (check-equal? 32472 (score (run-game (parse-decks "input"))))

  (check-equal? 291 (score (recursive-game (parse-decks "example") (list->set '()))))
  (check-equal? '((43 19) (2 29 14)) (recursive-game (parse-decks "infinite") (list->set '())))
  (check-equal? 36463 (score (recursive-game (parse-decks "input") (list->set '())))))

