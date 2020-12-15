(defun solve-next-lookup (last iter seen)
  (declare (type fixnum iter last))
  (declare (optimize (speed 3)))
  (let ((previous (gethash last seen 0)))
    (declare (type fixnum previous))
    (if (= previous 0)
        0
        (- iter previous))))

(defun solve-up-to (lst seen iter n)
  (declare (type list lst))
  (declare (type fixnum iter n))
  (declare (optimize (speed 3)))
  (if (= iter n)
      lst
      (let* ((last (car lst))
             (next (solve-next-lookup last iter seen)))
        (setf (gethash last seen) iter)
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

;; Somethign is wrong here, if I run the first example on a fresh repl instance,
;; I can get an answer in 3s, but trying to run it again hangs. If I try the
;; second example it just hangs. As comparison, the racket solution is
;; consistant at around 54s or so for a 30,000,000 iteration run. I don't know
;; if there is just a garbage collection limit or it optimizes on the second run
;; or what.
(defun run-test ()
  (princ "Test Example @ 30000000: ")
  (time (princ (= 175594 (car (solve-to-n (reverse '(0 3 6)) 30000000)))))
  (terpri)
  (princ "Second Star:")
  (time (princ (= 9007186 (car (solve-to-n (reverse '(0 5 4 1 10 14 7)) 30000000)))))
  (terpri))


