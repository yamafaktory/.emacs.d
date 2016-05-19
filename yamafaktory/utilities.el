;;; utilities.el --- custom functions

;;; Commentary:
;;; Additional custom functions.

;;; Code:

(defun clean ()
  "Kill all buffers except the current one and the special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun indent (spaces)
  "Change the js2-mode indentation based on the SPACES argument."
  (interactive "nPlease enter the new indentation size in spaces:")
  (setq-default js2-basic-offset spaces))

(provide 'utilities)

;;; utilities.el ends here
