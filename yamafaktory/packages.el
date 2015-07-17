;;; packages.el --- Extra packages to load

;; theme
(require-pkg 'cyberpunk-theme)
(load-theme 'cyberpunk t)

;; aggressive-indent
(require-pkg 'aggressive-indent)
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)

;; cider
(require-pkg 'cider)
(require 'cider)

;; clojure-cheatsheet
(require-pkg 'clojure-cheatsheet)
(require 'clojure-cheatsheet)
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

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
(add-hook 'after-init-hook #'global-flycheck-mode)

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

;; indent-guide
(require-pkg 'indent-guide)
(require 'indent-guide)
(indent-guide-global-mode)

;; json-mode
(require-pkg 'json-mode)
(require 'json-mode)

;; js2-mode
(require-pkg 'js2-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 4)

;; less-css-mode
(require-pkg 'less-css-mode)
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(setq less-css-indent-level 2)

;; magit
(require-pkg 'magit)
(require 'magit)

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

;; php-mode
(require-pkg 'php-mode)

;; projectile
(require-pkg 'projectile)
(require 'projectile)
(projectile-global-mode)

;; rust-lang
(require-pkg 'rust-mode)
(require 'rust-mode)

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
