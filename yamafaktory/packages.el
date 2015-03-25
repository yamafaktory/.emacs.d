;;; packages.el --- Extra packages to load

;; theme
(require-pkg 'gotham-theme)
(load-theme 'gotham t)

;; cider
(require-pkg 'cider)
(require 'cider)

;; clojure
(require-pkg 'clojure-mode)
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;; color-identifiers-mode
(require-pkg 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; company-mode
(require-pkg 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; dockerfile-mode
(require-pkg 'dockerfile-mode)
(require 'dockerfile-mode)

;; flycheck
(require-pkg 'flycheck)
(require 'flycheck)

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; helm
(require-pkg 'helm)
(require 'helm)
(global-set-key (kbd "C-²") 'helm-mini)

;; json-mode
(require-pkg 'json-mode)
(require 'json-mode)

;; js2-mode
(require-pkg 'js2-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; markdown-mode
(require-pkg 'markdown-mode)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; multiple-cursors
(require-pkg 'multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; multi-term
(require-pkg 'multi-term)
(require 'multi-term)

;; rainbow-delimiters
(require-pkg 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; smex
(require-pkg 'smex)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; yaml-mode
(require-pkg 'yaml-mode)
(require 'yaml-mode)

(provide 'packages)
