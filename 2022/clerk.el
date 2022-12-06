(require 'cider)

(defun clerk-show ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))

(defun clerk-clear-cache! ()
  (interactive)
  (cider-interactive-eval "(nextjournal.clerk/clear-cache!)")
  (message "Cache cleared!"))

(defun clerk-serve ()
  (interactive)
  (cider-interactive-eval
   "(nextjournal.clerk/serve! {:port 7777 :browse? true :watch-paths [\"src\"]})"))

(define-key cider-mode-map (kbd "<f8>") 'clerk-show)
(define-key cider-mode-map (kbd "S-<f8>") 'clerk-clear-cache!)

(provide 'clerk)
