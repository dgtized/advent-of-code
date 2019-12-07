#lang racket

(require racket/string)

(define (load-program)
  (let ((source (car (file->lines "input"))))
    (list->vector (map string->number (string-split source ",")))))

(define (print-memory memory)
  (println (string-join (map number->string (vector->list memory)) ",")))

(define (fetch memory offset)
  (vector-ref memory offset))

(define (store memory offset value)
  (vector-set! memory offset value)
  memory)

(define (match-operand operand)
  (case operand
    [(1) '(add 4)]
    [(2) '(multiply 4)]
    [(3) '(read 2)]
    [(4) '(write 2)]
    [(5) '(jump-if-true 3)]
    [(6) '(jump-if-false 3)]
    [(7) '(less-than 4)]
    [(8) '(equals 4)]
    [(99) '(halt 0)]))

(define (parameter-flags operand)
  (list
   (modulo (quotient operand 100) 10)
   (modulo (quotient operand 1000) 10)
   (modulo (quotient operand 10000) 10)))

(define (fetch-operand memory pc)
  (let ((operand (fetch memory pc)))
    (append (match-operand (modulo operand 100))
            (list (parameter-flags operand)))))

(define (parameter-value memory pc flags param)
  (let ((immediate (fetch memory (+ pc param))))
    (if (= 1 (list-ref flags (- param 1)))
        immediate
        (fetch memory immediate))))

(define debugging #f)
(define (step memory pc input output)
  (define (debug lst)
    (when debugging (println lst)))
  (match (fetch-operand memory pc)
    [(list 'add args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "addition " (+ a b) r))
       (step (store memory r (+ a b))
             (+ pc args)
             input output))]
    [(list 'multiply args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "multiply" (* a b) r))
       (step (store memory r (* a b))
             (+ pc args)
             input output))]
    [(list 'read args _)
     (if (pair? input)
         (let ((i (car input))
               (r (fetch memory (+ pc 1))))
           (debug (list pc "read" (car input) r))
           (step (store memory r i)
                 (+ pc args)
                 (cdr input) output))
         "read without input")]
    [(list 'write args flags)
     (let ((w (parameter-value memory pc flags 1)))
       (debug (list pc "write" w))
       (step memory (+ pc args) input (cons w output)))]
    [(list 'jump-if-true args flags)
     (let ((cnd (parameter-value memory pc flags 1))
           (jmp (parameter-value memory pc flags 2)))
       (debug (list pc "jump-if-true" cnd jmp))
       (step memory
             (if (> cnd 0) jmp (+ pc args))
             input output))]
    [(list 'jump-if-false args flags)
     (let ((cnd (parameter-value memory pc flags 1))
           (jmp (parameter-value memory pc flags 2)))
       (debug (list pc "jump-if-false" cnd jmp))
       (step memory
             (if (= cnd 0) jmp (+ pc args))
             input output))]
    [(list 'less-than args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "less-than" (if (< a b) 1 0) r))
       (step (store memory r (if (< a b) 1 0))
             (+ pc args)
             input output))]
    [(list 'equals args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "equals" (if (= a b) 1 0) r))
       (step (store memory r (if (= a b) 1 0))
             (+ pc args)
             input output))]
    [(list 'halt _ _)
     ;; (print-memory memory)
     (reverse output)]))

(let ((memory (load-program)))
  (step memory 0 '(6 0) '()))
