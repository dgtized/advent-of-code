#lang racket

(require racket/set)

(define (slurp-lines filename)
  (for/list ([line (file->lines filename)])
    (match-let ([(list _ ingredients allergens)
                 (regexp-match #rx"(.*) \\(contains (.*)\\)" line)])
      (list (regexp-split #rx" +" ingredients)
            (regexp-split #rx", " allergens)))))

(define (possible-sources parsed-input)
  (map (match-lambda
         [(list ingredients allergens)
          (for ([allergy allergens])
            (list allergy (list->set ingredients)))])
       parsed-input))

(possible-sources (slurp-lines "example"))

;; aborted

;; (slurp-lines "input")


