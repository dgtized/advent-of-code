(defun solve-next (lst)
  (let* ((previous (position (car lst) (cdr lst))))
    (if previous
        (cons (1+ previous) lst)
        (cons 0 lst))))

(defun solve-next-lookup (lst iter seen)
  (let ((previous (gethash (car lst) seen)))
    (princ (list iter 'lookup (car lst) previous)) (terpri)
    (if previous
        (- iter previous)
        0)))

(defun solve-up-to (lst seen iter n)
  (if (= iter n)
      lst
      (let* ((next (solve-next-lookup lst iter seen)))
        (setf (gethash (car lst) seen) iter)
        (princ (list iter 'set (car lst) iter)) (terpri)
        (solve-up-to (cons next lst) seen (1+ iter) n))))

(defun solve-to-n (lst n)
  (let ((seen (make-hash-table)))
    (solve-up-to lst
                 seen
                 (length lst)
                 n)))

(princ "Test example: ")
(princ (solve-to-n (reverse '(0 3 6)) 10)) (terpri)
(princ (reverse '(0 3 6 0 3 3 1 0 4 0))) (terpri)
;; (time (car (solve-up-to (reverse '(0 3 6)) 3 300000)))
;; (princ "First star: ")
;; (princ (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 2020)))
;; (terpri)
;; (time (princ (car (solve-up-to (reverse '(0 5 4 1 10 14 7)) 7 300000))))
