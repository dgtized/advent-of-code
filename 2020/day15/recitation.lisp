(declaim (ftype (function (list) list)))
(defun solve-next (lst)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((previous (position (car lst) (cdr lst))))
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

;; Tried making tail recusive and optimizing
(declaim (ftype (function (list fixnum fixnum) list) solve-up-to))
(defun solve-up-to (lst c n)
  (declare (optimize (speed 3) (safety 0)))
  (if (= c n)
      lst
      (solve-up-to (solve-next lst) (1+ c) n)))

(car (solve-to-n (reverse '(0 3 6)) 2020))
;; (time (car (solve-up-to (reverse '(0 3 6)) 3 300000)))
(princ "First star: ")
(princ (car (solve-up-to (reverse '(0 5 4 1 10 14 7)) 7 2020)))
;; (time (princ (car (solve-up-to (reverse '(0 5 4 1 10 14 7)) 7 300000))))
