;; https://rosettacode.org/wiki/Modular_inverse#Common_Lisp
(defun egcd (a b)
  (do ((r (cons b a) (cons (- (cdr r) (* (car r) q)) (car r))) ; (r+1 r) i.e. the latest is first.
       (s (cons 0 1) (cons (- (cdr s) (* (car s) q)) (car s))) ; (s+1 s)
       (u (cons 1 0) (cons (- (cdr u) (* (car u) q)) (car u))) ; (t+1 t)
       (q nil))
      ((zerop (car r)) (values (cdr r) (cdr s) (cdr u)))       ; exit when r+1 = 0 and return r s t
    (setq q (floor (/ (cdr r) (car r))))))

(defun invmod (a m)
  (multiple-value-bind (r s k) (egcd a m)
    (unless (= 1 r) (error "invmod: Values ~a and ~a are not coprimes." a m))
    s))

;; https://rosettacode.org/wiki/Chinese_remainder_theorem#Commonlisp
(defun chinese-remainder (am)
  "Calculates the Chinese Remainder for the given set of integer modulo pairs.
 Note: All the ni and the N must be coprimes."
  (loop :for (a . m) :in am
        :with mtot = (reduce #'* (mapcar #'(lambda(X) (cdr X)) am))
        :with sum  = 0
        :finally (return (mod sum mtot))
        :do
           (incf sum (* a (invmod (/ mtot m) m) (/ mtot m)))))

;; Encoded input from problem 13 copied from javascript as javascript solution
;; is off by ~13 for some reason, presumably due to small numeric error?
(let* ((input '((0  . 17)  (11 . 37) (17 . 739)
                (19 . 29)  (30 . 13) (40 . 23)
                (48 . 971) (58 . 41) (67 . 19)))
       (product (reduce #'* (mapcar #'(lambda (x) (cdr x)) input))))
  (- product (chinese-remainder input)))
;; => 535296695251210
