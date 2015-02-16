;;; packages.el --- Extra packages to load

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; smex
(require-pkg 'smex)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ample
(require-pkg 'ample-theme)
(load-theme 'ample t)
(enable-theme 'ample)

;; rainbow-delimiters
(require-pkg 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; multiple-cursors
(require-pkg 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; clojure
(require-pkg 'clojure-mode)
(require 'clojure-mode)

;; cider
(require-pkg 'cider)
(require 'cider)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; js2-mode
(require-pkg 'js2-mode)
(require 'js2-mode)

;; multi-term
(require-pkg 'multi-term)
(require 'multi-term)

;; dockerfile-mode
(require-pkg 'dockerfile-mode)
(require 'dockerfile-mode)

(provide 'packages)
