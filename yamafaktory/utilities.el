;;; utilities.el --- custom functions

(defun change-js-indent (spaces)
  "Change the javascript indentation."
  (interactive "p")
  (setq-default js2-basic-offset spaces))

(provide 'utilities)
