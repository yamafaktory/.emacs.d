;;; packages.el --- Extra packages to load

;; theme
(require-pkg 'gotham-theme)
(load-theme 'gotham t)

;; diminish
(require-pkg 'diminish)

;; aggressive-indent
(require-pkg 'aggressive-indent t)
(global-aggressive-indent-mode t)

;; anzu
(require-pkg 'anzu t)
(diminish 'anzu-mode)
(global-anzu-mode)

;; browse-at-remote
(require-pkg 'browse-at-remote t)
(global-set-key (kbd "C-c r") 'browse-at-remote)

;; cider
(require-pkg 'cider)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

;; clojure-cheatsheet
(require-pkg 'clojure-cheatsheet t)
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet))

;; clojure-mode
(require-pkg 'clojure-mode t)
(add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))

;; color-identifiers-mode
(require-pkg 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; company-mode
(require-pkg 'company t)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-dabbrev-downcase nil)

;; dockerfile-mode
(require-pkg 'dockerfile-mode t)

;; flycheck
(require-pkg 'flycheck t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; flycheck-haskell
(require-pkg 'flycheck-haskell t)
(with-eval-after-load "flycheck"
  (with-eval-after-load "haskell"
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe t)
(global-git-gutter-mode t)

;; golden-ratio
(require-pkg 'golden-ratio t)
(golden-ratio-mode 1)

;; haskell-mode
(require-pkg 'haskell-mode t)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-process-type 'stack-ghci
      haskell-compile-cabal-build-command "stack build"
      haskell-interactive-popup-errors nil)
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-switch)
(define-key haskell-mode-map (kbd "C-c b") 'haskell-process-load-file)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
(define-key haskell-mode-map (kbd "C-c i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c t") 'haskell-process-do-type)

;; helm
(require-pkg 'helm t)
(global-set-key (kbd "M-x") 'helm-M-x)

;; helm-ag
(require-pkg 'helm-ag t)

;; helm-projectile
(require-pkg 'helm-projectile t)
(helm-projectile-on)

;; indent-guide
(require-pkg 'indent-guide t)
(indent-guide-global-mode)

;; inf-clojure
(require-pkg 'inf-clojure t)
(defun figwheel-repl ()
  (interactive)
  (run-clojure "lein figwheel"))
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

;; json-mode
(require-pkg 'json-mode t)

;; js2-mode
(require-pkg 'js2-mode t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-basic-offset 4)
(setq-default js2-indent-switch-body t)
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

;; less-css-mode
(require-pkg 'less-css-mode t)
(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))
(setq less-css-indent-level 2)

;; magit
(require-pkg 'magit t)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode
(require-pkg 'markdown-mode t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; midnight
(require-pkg 'midnight t)

;; multiple-cursors
(require-pkg 'multiple-cursors t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; multi-term
(require-pkg 'multi-term t)
(setq multi-term-program "/bin/zsh")

;; php-mode
(require-pkg 'php-mode)

;; projectile
(require-pkg 'projectile t)
(projectile-global-mode)
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; rainbow-delimiters
(require-pkg 'rainbow-delimiters t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

;; undo-tree
(require-pkg 'undo-tree t)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; windmove
(require-pkg 'windmove t)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; yaml-mode
(require-pkg 'yaml-mode t)

(provide 'packages)
