#lang racket

(require racket/string)
(require racket/match)

(define (number->binary-string n)
  (~r n #:base 2 #:min-width 36 #:pad-string "0"))

(define (binary-string->number str)
  (string->number str 2))

(define (load-program filename)
  (for/list ([line (file->lines filename)])
    (match line
      [(pregexp "mask = (.+)$" (list _ m))
       (list 'mask m)]
      [(pregexp "mem\\[(\\d+)\\] = (\\d+)$" (list _ address value))
       (list 'mem
             (string->number address)
             (number->binary-string (string->number value)))])))

(define (apply-mask n mask)
  (list->string
   (for/list ([e (in-string n)]
              [m (in-string mask)])
     (match m
       [#\X e]
       [#\1 #\1]
       [#\0 #\0]))))

(define (add-to-all elem lists)
  (if (empty? lists)
      empty
      (cons (cons elem (first lists))
            (add-to-all elem (rest lists)))))

(define (generate-combinations pattern)
  (if (empty? pattern)
      (list empty)
      (let ((remaining (generate-combinations (rest pattern))))
        (match (first pattern)
          [(list elems ...)
           (apply append
                  (map (lambda (e) (add-to-all e remaining))
                       elems))]
          [elem (add-to-all elem remaining)]))))

(define (expanded-addresses address mask)
  (map list->string
       (generate-combinations
        (for/list ([a (in-string address)]
                   [m (in-string mask)])
          (match m
            [#\X '(#\0 #\1)]
            [#\1 #\1]
            [#\0 a])))))

(define (decoded-addresses address mask)
  (map binary-string->number
       (expanded-addresses (number->binary-string address) mask)))

(define (bitmask-set memory mask address value)
  (hash-set memory address (apply-mask value mask)))

(define (decoder-set memory mask address value)
  (apply hash-set*
         (cons memory
               (foldr (lambda (addr result) (cons addr (cons value result)))
                      '()
                      (decoded-addresses address mask)))))

(define (run-program mem-set program)
  (define (runner memory mask program)
    (if (null? program)
        memory
        (match (first program)
          [(list 'mem address value)
           (runner (mem-set memory mask address value)
                   mask
                   (rest program))]
          [(list 'mask new-mask)
           (runner memory new-mask (rest program))])))

  (runner (make-immutable-hash) (make-string 36 #\X) program))

(define (memory-sum memory)
  (apply + (map binary-string->number (hash-values memory))))

(define (first-star filename)
  (memory-sum (run-program bitmask-set (load-program filename))))

(define (second-star filename)
  (memory-sum (run-program decoder-set (load-program filename))))

(module+ test
  (require rackunit)

  (check-equal? (add-to-all 'x (list empty))
                '((x)))
  (check-equal? (add-to-all 'x (list '(a) '(b)))
                '((x a) (x b)))

  (check-equal? (generate-combinations '((0 1) 0 1))
                '((0 0 1)
                  (1 0 1)))
  ;; not needed for problem, but a good generalization
  (check-equal? (generate-combinations '((0 1 2) 0 1))
                '((0 0 1)
                  (1 0 1)
                  (2 0 1)))
  (check-equal? (generate-combinations '((0 1) 0 (0 1)))
                '((0 0 0)
                  (0 0 1)
                  (1 0 0)
                  (1 0 1)))

  (check-equal? (apply-mask "01100101" "X1XXXX0X") "01100101")
  (check-equal? (apply-mask "00001011" "X1XXXX0X") "01001001")

  (check-equal?
   (expanded-addresses (number->binary-string 42)
                       "000000000000000000000000000000X1001X")
   '("000000000000000000000000000000011010"
     "000000000000000000000000000000011011"
     "000000000000000000000000000000111010"
     "000000000000000000000000000000111011"))

  (check-equal? (load-program "example")
                '((mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
                  (mem 8 "000000000000000000000000000000001011")
                  (mem 7 "000000000000000000000000000001100101")
                  (mem 8 "000000000000000000000000000000000000")))

  (check-equal? (run-program bitmask-set (load-program "example"))
                '#hash((7 . "000000000000000000000000000001100101")
                       (8 . "000000000000000000000000000001000000")))

  (check-equal? (first-star "example") 165)
  (check-equal? (first-star "input") 14954914379452)

  (check-equal? (run-program decoder-set (load-program "floating"))
                '#hash((16 . "000000000000000000000000000000000001")
                       (17 . "000000000000000000000000000000000001")
                       (18 . "000000000000000000000000000000000001")
                       (19 . "000000000000000000000000000000000001")
                       (24 . "000000000000000000000000000000000001")
                       (25 . "000000000000000000000000000000000001")
                       (26 . "000000000000000000000000000000000001")
                       (27 . "000000000000000000000000000000000001")
                       (58 . "000000000000000000000000000001100100")
                       (59 . "000000000000000000000000000001100100")))

  (check-equal? (second-star "floating") 208)
  (check-equal? (second-star "input") 3415488160714))
