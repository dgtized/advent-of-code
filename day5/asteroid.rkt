#lang racket

(require racket/string)

(define (load-program)
  (list->vector (string-split (car (file->lines "input")) ",")))

(define (match-operand operand)
  (case operand
    [(1) '(add 4)]
    [(2) '(multiply 4)]
    [(3) '(read 2)]
    [(4) '(write 2)]
    [(99) '(halt 0)]))

(define (parse-operand operand)
  (let* ((chars (string->list operand)))
    (if (<= (length chars) 2)
        (append (match-operand (string->number operand))
                (list '(0 0 0)))
        (append (match-operand
                 (string->number (list->string (take-right chars 2))))
                (list
                 (match (map (lambda (n) (if (eq? n #\1) 1 0))
                                 (drop-right chars 2))
                       [(list) (list 0 0 0)]
                       [(list c) (list 0 0 c)]
                       [(list b c) (list 0 b c)]
                       [(list a b c) (list a b c)]))))))

(define (fetch memory offset)
  (string->number (vector-ref memory offset)))

(define (store memory offset value)
  (vector-set! memory offset (number->string value))
  memory)

(define (parameter-value memory pc flags param)
  (let ((immediate (fetch memory (+ pc param))))
    (if (= 1 (list-ref (reverse flags) (- param 1)))
        immediate
        (fetch memory immediate))))

(define (step memory pc input output)
  (match-let ([(list op args flags)
               (parse-operand (vector-ref memory pc))])
    (cond [(eqv? op 'add)
           (let ((a (parameter-value memory pc flags 1))
                 (b (parameter-value memory pc flags 2))
                 (r (fetch memory (+ pc 3))))
             (println (list pc "addition " (+ a b) r))
             (step (store memory r (+ a b))
                   (+ pc args)
                   input output))]
          [(eqv? op 'multiply)
           (let ((a (parameter-value memory pc flags 1))
                 (b (parameter-value memory pc flags 2))
                 (r (fetch memory (+ pc 3))))
             (println (list pc "multiply" (* a b) r))
             (step (store memory r (* a b))
                   (+ pc args)
                   input output))]
          [(eqv? op 'read)
           (let ((i (car input))
                 (r (fetch memory (+ pc 1))))
             (println (list pc "read" (car input) r))
             (step (store memory r i)
                   (+ pc args)
                   (cdr input) output))]
          [(eqv? op 'write)
           (let ((w (parameter-value memory pc flags 1)))
             (println (list pc "write" w))
             (step memory (+ pc args) input (cons w output)))]
          [(eqv? op 'halt)
           (println (reverse output))
           (list memory pc input output)])))

(let ((memory (load-program)))
  ;; (println (fetch memory 255))
  (step memory 0 '(1) '()))
