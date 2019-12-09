#lang racket

(require racket/string)

(define (load-program filename)
  (let ((source (car (file->lines filename))))
    (list->vector (map string->number (string-split source ",")))))

(struct cpu (memory pc input output condition))

(define (print-memory cpu)
  (println (list "pc:" (cpu-pc cpu)
                 " input: " (cpu-input cpu)
                 " output: " (cpu-output cpu)
                 " condition: " (cpu-condition cpu)))
  (println (string-join (map number->string (vector->list (cpu-memory cpu))) ",")))

(define (fetch memory offset)
  (vector-ref memory offset))

(define (cpu-fetch cpu offset)
  (vector-ref (cpu-memory cpu) offset))

(define (store memory offset value)
  (let ((newmem (vector-copy memory)))
    (vector-set! newmem offset value)
    newmem))

;; (define (cpu-store cpu offset value)
;;   (store (cpu-memory cpu) offset value))

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

(define (read-port! ports port)
  (let ((channel (vector-ref ports port)))
    (if (empty? channel)
        'blocked
        (let ((value (car channel)))
          (vector-set! ports port (cdr channel))
          value))))

(define (write-port! ports port value)
  (let ((channel (vector-ref ports port)))
    (vector-set! ports port (append channel (list value)))))

(define debugging #f)
(define (step machine ports)
  (define (debug lst)
    (when debugging (println lst)))

  (match-define (cpu memory pc input output condition) machine)
  (match (fetch-operand memory pc)
    [(list 'add args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "addition " (+ a b) r))
       (struct-copy cpu machine
                    [memory (store memory r (+ a b))]
                    [pc (+ pc args)]))]
    [(list 'multiply args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "multiply" (* a b) r))
       (struct-copy cpu machine
                    [memory (store memory r (* a b))]
                    [pc (+ pc args)]))]
    [(list 'read args _)
     (let ((value (read-port! ports input)))
       (if (eq? value 'blocked)
           (struct-copy cpu machine [condition 'read])
           (let ((r (fetch memory (+ pc 1))))
             (debug (list pc "read" value r))
             (struct-copy cpu machine
                          [memory (store memory r value)]
                          [pc (+ pc args)]))))]
    [(list 'write args flags)
     (let ((value (parameter-value memory pc flags 1)))
       (debug (list pc "write" value))
       (write-port! ports output value)
       (struct-copy cpu machine
                    [pc (+ pc args)]))]
    [(list 'jump-if-true args flags)
     (let ((cnd (parameter-value memory pc flags 1))
           (jmp (parameter-value memory pc flags 2)))
       (debug (list pc "jump-if-true" cnd jmp))
       (struct-copy cpu machine
                    [pc (if (> cnd 0) jmp (+ pc args))]))]
    [(list 'jump-if-false args flags)
     (let ((cnd (parameter-value memory pc flags 1))
           (jmp (parameter-value memory pc flags 2)))
       (debug (list pc "jump-if-false" cnd jmp))
       (struct-copy cpu machine
                    [pc (if (= cnd 0) jmp (+ pc args))]))]
    [(list 'less-than args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "less-than" (if (< a b) 1 0) r))
       (struct-copy cpu machine
                    [memory (store memory r (if (< a b) 1 0))]
                    [pc (+ pc args)]))]
    [(list 'equals args flags)
     (let ((a (parameter-value memory pc flags 1))
           (b (parameter-value memory pc flags 2))
           (r (fetch memory (+ pc 3))))
       (debug (list pc "equals" (if (= a b) 1 0) r))
       (struct-copy cpu machine
                    [memory (store memory r (if (= a b) 1 0))]
                    [pc (+ pc args)]))]
    [(list 'halt _ _) (struct-copy cpu machine [condition 'halt])]))

(define (run-until-blocked machine ports)
  (match machine
    [(cpu _ _ _ _ 'run)
     (run-until-blocked (step machine ports) ports)]
    [(cpu _ _ _ _ 'read)
     (struct-copy cpu machine
                  [condition 'run])]
    [(cpu _ _ _ _ 'halt)
     machine]))

(define (step-all machines ports)
  (map (lambda (m) (run-until-blocked m ports)) machines))

(define (any-running? machines)
  (ormap (lambda (m) (eq? 'run (cpu-condition m)))
          machines))

(define (run-all machines ports)
  (if (any-running? machines)
      (run-all (step-all machines ports) ports)
      machines))
