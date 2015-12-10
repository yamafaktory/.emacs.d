;;; utilities.el --- custom functions

(defun change-js-indent (spaces)
  "Change the javascript indentation."
  (interactive "p")
  (setq-default js2-basic-offset spaces))

(defun kill-other-buffers ()
  "Kill all buffers except current and special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(provide 'utilities)
