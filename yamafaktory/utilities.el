;;; utilities.el --- custom functions

;;; Commentary:
;;; Additional custom functions.

;;; Code:

(defun indent (spaces)
  "Change the web-mode indentation."
  (interactive "nPlease enter the new indentation size in spaces:")
  (setq web-mode-css-indent-offset spaces)
  (setq web-mode-code-indent-offset spaces)
  (setq web-mode-markup-indent-offset spaces))

(defun clean ()
  "Kill all buffers except the current one and the special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(provide 'utilities)

;;; utilities.el ends here
