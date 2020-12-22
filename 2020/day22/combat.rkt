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

(define (score-deck deck)
  (for/sum ([i (range 1 (+ 1 (length deck)))]
            [v (reverse deck)])
    (* i v)))

(define (deck-hash decks)
  (score-deck (flatten decks)))

(define (recursive-game decks seen)
  (define (recursive-round decks seen)
    (match-let* (((list p1 p2) decks)
                 ((list c1 r1 ...) p1)
                 ((list c2 r2 ...) p2))
      ;; (println (list "round" c1 (rest p1) c2 (rest p2)))
      (cond ((set-member? seen decks)
             ;; (println (list "player 1 wins" decks))
             (list (append r1 (list c1 c2)) r2))
            ((and (<= c1 (length r1))
                  (<= c2 (length r2)))
             (match-let (((list s1 s2)
                         (recursive-game (list (take r1 c1)
                                               (take r2 c2))
                                         (list->set '()))))
              (if (> (length s1) (length s2))
                  (list (append r1 (list c1 c2)) r2)
                  (list r1 (append r2 (list c2 c1))))))
            (else (normal-round decks)))))

  ;; (println (list "recursion" decks (set-count seen)))
  (if (or (ormap empty? decks))
      decks
      (let ((new-deck (recursive-round decks seen)))
        (recursive-game new-deck (set-add seen decks)))))

(define (score decks)
  (let ((winner (flatten decks)))
    (score-deck winner)))

;; (= 306 (score (run-game (parse-decks "example"))))
;; (= 32472 (score (run-game (parse-decks "input"))))

;; (set-member? (list->set '(7254)) (deck-hash (parse-decks "example")))

;; (= 291 (score (recursive-game (parse-decks "example") (list->set '()))))
;; (= 273 (score (recursive-game (parse-decks "infinite") (list->set '()))))
;; (score (recursive-game (parse-decks "input") (list->set '())))
