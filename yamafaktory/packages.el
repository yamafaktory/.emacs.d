;;; packages.el --- Extra packages to load

;;; Commentary:
;;; Required packages and their configuration.

;;; Code:

;; theme
(require-pkg 'dracula-theme)
(load-theme 'dracula t)

;; diminish
(require-pkg 'diminish)

;; aggressive-indent
(require-pkg 'aggressive-indent t)
(global-aggressive-indent-mode t)
(add-to-list 'aggressive-indent-excluded-modes 'dockerfile-mode)
(add-to-list 'aggressive-indent-excluded-modes 'elm-mode)
(add-to-list 'aggressive-indent-excluded-modes 'sh-mode)

;; browse-at-remote
(require-pkg 'browse-at-remote t)
(global-set-key (kbd "C-c r") 'browse-at-remote/browse)

;; buffer-move
(require-pkg 'buffer-move t)
(global-set-key (kbd "C-M-j") 'buf-move-left)
(global-set-key (kbd "C-M-l") 'buf-move-right)
(global-set-key (kbd "C-M-i") 'buf-move-up)
(global-set-key (kbd "C-M-k") 'buf-move-down)

;; color-identifiers-mode
(require-pkg 'color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; company-math
(require-pkg 'company-math t)
(add-to-list 'company-backends 'company-math-symbols-unicode)

;; company-mode
(require-pkg 'company t)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-dabbrev-downcase nil)

;; counsel
(require-pkg 'counsel t)

;; dockerfile-mode
(require-pkg 'dockerfile-mode t)

;; ebal
(require-pkg 'ebal t)
(setq-default ebal-operation-mode 'stack)
(global-set-key (kbd "C-c e") 'ebal-execute)

;; flycheck
(require-pkg 'flycheck t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe t)
(global-git-gutter-mode t)

;; golden-ratio
(require-pkg 'golden-ratio t)
(golden-ratio-mode 1)

;; highlight-numbers
(require-pkg 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; indent-guide
(require-pkg 'indent-guide t)
(indent-guide-global-mode)

;; intero
(require-pkg 'intero t)
(add-hook 'haskell-mode-hook 'intero-mode)

;; ivy
(require-pkg 'ivy t)
(ivy-mode 1)
(setq-default ivy-use-virtual-buffers t)
(global-set-key "\C-s"          'counsel-grep-or-swiper)
(global-set-key (kbd "C-x C-r") 'ivy-resume)
(global-set-key (kbd "M-x")     'counsel-M-x)
(global-set-key (kbd "C-x C-a") 'counsel-ag)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; js-doc
(require-pkg 'js-doc)
(add-hook 'js2-mode-hook
          #'(lambda ()
              (define-key js2-mode-map "C-x C-d" 'js-doc-insert-function-doc)
              (define-key js2-mode-map "@" 'js-doc-insert-tag)))

;; json-mode
(require-pkg 'json-mode t)

;; js2-mode
(require-pkg 'js2-mode t)
(add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(setq-default js2-basic-offset 2
              js2-indent-switch-body t
              js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

;; less-css-mode
(require-pkg 'less-css-mode t)
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;; magit
(require-pkg 'magit t)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode
(require-pkg 'markdown-mode t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))

;; midnight
(require-pkg 'midnight t)

;; multiple-cursors
(require-pkg 'multiple-cursors t)
(global-set-key (kbd "C-S-c C-S-c")   'mc/edit-lines)
(global-set-key (kbd "C->")           'mc/mark-next-like-this)
(global-set-key (kbd "C-<")           'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")       'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; multi-term
(require-pkg 'multi-term t)
(setq-default multi-term-program "/bin/zsh")

;; octave
(global-set-key (kbd "C-c o") 'run-octave)

;; php-mode
(require-pkg 'php-mode)

;; rainbow-delimiters
(require-pkg 'rainbow-delimiters t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; smart-mode-line
(require-pkg 'smart-mode-line t)
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'respectful)
(sml/setup)

;; undo-tree
(require-pkg 'undo-tree t)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; windmove
(require-pkg 'windmove t)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)

;; yaml-mode
(require-pkg 'yaml-mode t)

(provide 'packages)

;;; packages.el ends here
