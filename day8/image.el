(require 'f)

(defun image->layers (filename)
  (let* ((image (s-trim (f-read-text filename)))
         (layerw (* 25 6)))
    (cl-loop for i from 0 upto (- (length image) layerw) by layerw
             collect (substring image i (+ i layerw)))))


;; (s-count-matches "0" "00120") => 3

(let ((layers (image->layers "input")))
  (car (cl-sort
        (cl-loop for layer in layers
                 collect (list (s-count-matches "0" layer)
                               (* (s-count-matches "1" layer)
                                  (s-count-matches "2" layer)))
                 )
        '< :key 'car)))



