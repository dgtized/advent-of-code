(require 'f)

(defvar image-width 25)
(defvar image-height 6)
(defvar layer-width (* image-width image-height))

(defun image->layers (filename)
  (let* ((image (s-trim (f-read-text filename))))
    (cl-loop for i from 0 below (length image) by layer-width
             collect (substring image i (+ i layer-width)))))

;; (s-count-matches "0" "00120") => 3

(defun image-checksum ()
  (let ((layers (image->layers "input")))
    (car (cl-sort
          (cl-loop for layer in layers
                   collect (list (s-count-matches "0" layer)
                                 (* (s-count-matches "1" layer)
                                    (s-count-matches "2" layer)))
                   )
          '< :key 'car))))

;; (image-checksum)

(defun transposed-pixels ()
  (cl-loop for i below layer-width
           collect (cl-loop for layer in (image->layers "input")
                            collect (substring layer i (1+ i)))))

(defun visible-layer ()
  (cl-loop for pixels in (transposed-pixels)
           concat (car (seq-drop-while (lambda (pixel) (equalp "2" pixel)) pixels))))


(with-current-buffer (pop-to-buffer "*image output*")
  (erase-buffer)
  (let ((visible (visible-layer)))
    (cl-loop for i below layer-width by image-width
             do (insert (substring visible i (+ i image-width)) "\n"))))
