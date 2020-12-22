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

(define (recursive-game decks seen)
  (let ((deck-hash (score decks)))
    (cond ((or (ormap empty? decks))
           (println (list "win by empty" decks))
           decks)
          ((set-member? seen deck-hash)
           (println (list "player 1 wins by previous" decks))
           (list (first decks) '()))
          (else
           (let ((new-deck
                  (match-let* (((list p1 p2) decks)
                               ((list c1 r1 ...) p1)
                               ((list c2 r2 ...) p2))
                    (println (list "recursion" (set-count seen) p1 p2))
                    (cond ((and (<= c1 (length r1))
                                (<= c2 (length r2)))
                           (println (list "Starting recursive-game" (set-count seen)))
                           (match-let (((list s1 s2)
                                        (recursive-game (list (take r1 c1)
                                                              (take r2 c2))
                                                        (list->set '()))))
                             (println (list "Exiting recursive-game" (set-count seen)))
                             (cond ((empty? s2)
                                    (list (append r1 (list c1 c2)) r2))
                                   ((empty? s1)
                                    (list r1 (append r2 (list c2 c1))))
                                   (else (list (append r1 (list c1 c2)) r2)))))
                          (else (normal-round decks))))))
             (recursive-game new-deck (set-add seen deck-hash)))))))

;; (= 306 (score (run-game (parse-decks "example"))))
;; (= 32472 (score (run-game (parse-decks "input"))))

;; (= 291 (score (recursive-game (parse-decks "example") (list->set '()))))
;; (= 273 (score (recursive-game (parse-decks "infinite") (list->set '()))))
;; (score (recursive-game (parse-decks "input") (list->set '())))

(score (first '((6 13 23 9 2 15 19 10 29 14 41 3 50 42 16 5 37 24 32 22 4 20 46 30 12 1 48 38) (43 28 40 34 27 26 47 8 45 17 44 7 31 25 39 33 49 36 35 21 18 11))))
