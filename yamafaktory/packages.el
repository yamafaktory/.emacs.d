;;; packages.el --- Extra packages to load

;; theme
(require-pkg 'sublime-themes)
(load-theme 'brin t)

;; cider
(require-pkg 'cider)
(require 'cider)

;; clojure-mode
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
(global-set-key (kbd "M-x") 'helm-M-x)

;; helm-projectile
(require-pkg 'helm-projectile)
(require 'helm-projectile)
(helm-projectile-on)

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

;; projectile
(require-pkg 'projectile)
(require 'projectile)
(projectile-global-mode)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; yaml-mode
(require-pkg 'yaml-mode)
(require 'yaml-mode)

(provide 'packages)
