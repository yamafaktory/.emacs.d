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

(defun custom-web-mode-hook (spaces)
  "Custom hooks for web-mode, indentation is based on the SPACES argument."
  (setq-default js2-basic-offset spaces
                web-mode-css-indent-offset spaces
                web-mode-code-indent-offset spaces
                web-mode-markup-indent-offset spaces
                web-mode-enable-auto-pairing nil
                web-mode-enable-css-colorization t
                web-mode-enable-current-element-highlight t))

(defun indent (spaces)
  "Change the web-mode indentation based on the SPACES argument."
  (interactive "nPlease enter the new indentation size in spaces:")
  (remove-hook 'web-mode-hook 'custom-web-mode-hook)
  (add-hook 'web-mode-hook (custom-web-mode-hook spaces))
  (web-mode))

(provide 'utilities)

;;; utilities.el ends here
