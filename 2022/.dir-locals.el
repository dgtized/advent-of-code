((nil
  (eval . ((lambda () (when (not (featurep 'clerk))
                   (let ((clerk-file (expand-file-name "clerk.el" default-directory)))
                     (when (file-exists-p clerk-file)
                       (load clerk-file)
                       (require 'clerk)))))))))
