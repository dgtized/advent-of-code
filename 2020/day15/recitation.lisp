(defun solve-next (lst)
  (let* ((last (car lst))
         (previous (position last (cdr lst))))
    (if previous
        (cons (1+ previous) lst)
        (cons 0 lst))))

;; (solve-next (reverse '(0 3 6))) ;; 0
;; (solve-next (reverse '(0 3 6 0))) ;; 3
;; (solve-next (reverse '(0 3 6 0 3))) ;; 3
;; (solve-next (reverse '(0 3 6 0 3 3))) ;; 1
;; (solve-next (reverse '(0 3 6 0 3 3 1))) ;; 0
;; (solve-next (reverse '(0 3 6 0 3 3 1 0))) ;; 4
;; (solve-next (reverse '(0 3 6 0 3 3 1 0 4))) ;; 0

(defun solve-to-n (lst n)
  (if (= n (length lst))
      lst
      (solve-next (solve-to-n lst (1- n)))))

(car (solve-to-n (reverse '(0 3 6)) 2020))
(princ "First star: ")
(princ (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 2020)))
