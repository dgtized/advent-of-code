(defun solve-next-lookup (lst iter seen)
  (let ((previous (gethash (car lst) seen)))
    (if previous
        (- iter previous)
        0)))

(defun solve-up-to (lst seen iter n)
  (if (= iter n)
      lst
      (let* ((next (solve-next-lookup lst iter seen)))
        (setf (gethash (car lst) seen) iter)
        (solve-up-to (cons next lst) seen (1+ iter) n))))

(defun solve-to-n (lst n)
  (let ((seen (make-hash-table)))
    (loop for i from 1 to (length lst)
          for elem in (reverse (cdr lst))
          do (setf (gethash elem seen) i))
    (solve-up-to lst
                 seen
                 (length lst)
                 n)))

(princ "Test example @ 2020: ")
(princ (car (solve-to-n (reverse '(0 3 6)) 2020))) (terpri)

(princ "First star: ")
(princ (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 2020)))
(terpri)

(defun run-test ()
  (princ "Test Example @ 30000000: ")
  (time (princ (= 175594 (car (solve-to-n (reverse '(0 3 6)) 30000000)))))
  (terpri)
  (princ "Second Star:")
  (time (princ (= 9007186 (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 30000000)))))
  (terpri))


