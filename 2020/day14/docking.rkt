#lang racket

(require racket/string)
(require racket/match)

(define (apply-mask n mask)
  (list->string
   (for/list ([e (in-string n)]
              [m (in-string mask)])
     (match m
       [#\X e]
       [#\1 #\1]
       [#\0 #\0]))))

(define (load-program filename)
  (for/list ([line (file->lines filename)])
    (match line
      [(pregexp "mask = (.+)$" (list _ m))
       (list 'mask m)]
      [(pregexp "mem\\[(\\d+)\\] = (\\d+)$" (list _ address value))
       (list 'mem
             (string->number address)
             (~r (string->number value) #:base 2 #:min-width 36 #:pad-string "0"))])))

(define (bitmask-set memory mask address value)
  (hash-set memory address (apply-mask value mask)))

(define (run-program memory mem-set mask program)
  (if (null? program)
      memory
      (match (first program)
        [(list 'mem address value)
         (run-program (mem-set memory mask address value)
                      mem-set
                      mask
                      (rest program))]
        [(list 'mask new-mask)
         (run-program memory mem-set new-mask (rest program))])))

(define (memory-sum memory)
  (apply + (map (lambda (x) (string->number x 2)) (hash-values memory))))

(define (first-star filename)
  (memory-sum (run-program (make-immutable-hash)
                           bitmask-set
                           (make-string 36 #\X)
                           (load-program filename))))

(module+ test
  (require rackunit)
  (check-equal? (apply-mask "01100101" "X1XXXX0X") "01100101")
  (check-equal? (apply-mask "00001011" "X1XXXX0X") "01001001")

  (check-equal? (load-program "example")
                '((mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
                  (mem 8 "000000000000000000000000000000001011")
                  (mem 7 "000000000000000000000000000001100101")
                  (mem 8 "000000000000000000000000000000000000")))

  (check-equal? (run-program (make-immutable-hash)
                             bitmask-set
                             (make-string 36 #\X)
                             (load-program "example"))
                '#hash((7 . "000000000000000000000000000001100101")
                       (8 . "000000000000000000000000000001000000")))

  (check-equal? (first-star "example") 165)
  (check-equal? (first-star "input") 14954914379452))
