;;; packages.el --- Extra packages to load

;; theme
(require-pkg 'cyberpunk-theme)
(load-theme 'cyberpunk t)

;; diminish
(require-pkg 'diminish)

;; aggressive-indent
(require-pkg 'aggressive-indent)
(global-aggressive-indent-mode 1)

;; cider
(require-pkg 'cider)

;; clojure-cheatsheet
(require-pkg 'clojure-cheatsheet)
(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

;; clojure-mode
(require-pkg 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;; color-identifiers-mode
(require-pkg 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; company-mode
(require-pkg 'company)
(diminish 'company-mode)

;; dockerfile-mode
(require-pkg 'dockerfile-mode)

;; flycheck
(require-pkg 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe)
(global-git-gutter-mode t)

;; golden-ratio
(require-pkg 'golden-ratio)
(golden-ratio-mode 1)

;; helm
(require-pkg 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;; helm-ag
(require-pkg 'helm-ag)

;; helm-projectile
(require-pkg 'helm-projectile)
(helm-projectile-on)

;; indent-guide
(require-pkg 'indent-guide)
(indent-guide-global-mode)

;; json-mode
(require-pkg 'json-mode)

;; js2-mode
(require-pkg 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 4)

;; less-css-mode
(require-pkg 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(setq less-css-indent-level 2)

;; magit
(require-pkg 'magit)

;; markdown-mode
(require-pkg 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; multiple-cursors
(require-pkg 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; multi-term
(require-pkg 'multi-term)

;; php-mode
(require-pkg 'php-mode)

;; projectile
(require-pkg 'projectile)
(projectile-global-mode)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; rust-lang
(require-pkg 'rust-mode)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; undo-tree
(require-pkg 'undo-tree)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; yaml-mode
(require-pkg 'yaml-mode)

(provide 'packages)
