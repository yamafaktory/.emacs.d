;;; utilities.el --- custom functions

;;; Commentary:
;;; Additional custom functions.

;;; Code:

(defun js-indent (spaces)
  "Change the javascript indentation."
  (interactive "nPlease enter the new indentation size in spaces:")
  (setq-default js2-basic-offset spaces))

(defun clean-buffers ()
  "Kill all buffers except current and special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(provide 'utilities)

;;; utilities.el ends here
