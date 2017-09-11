;;; packages.el --- Extra packages to load

;;; Commentary:
;;; Required packages and their configuration.

;;; Code:

;; add-node-modules-path
(require-pkg 'add-node-modules-path)
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'add-node-modules-path))

;; browse-at-remote
(require-pkg 'browse-at-remote t)
(global-set-key (kbd "C-c r") 'bar-browse)

;; buffer-move
(require-pkg 'buffer-move t)
(global-set-key (kbd "C-M-j") 'buf-move-left)
(global-set-key (kbd "C-M-l") 'buf-move-right)
(global-set-key (kbd "C-M-i") 'buf-move-up)
(global-set-key (kbd "C-M-k") 'buf-move-down)

;; company-math
(require-pkg 'company-math t)
(add-to-list 'company-backends 'company-math-symbols-unicode)

;; company-mode
(require-pkg 'company t)
(add-hook 'after-init-hook 'global-company-mode)
(setq-default company-dabbrev-downcase nil)
(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; counsel
(require-pkg 'counsel t)

;; diminish
(require-pkg 'diminish)

;; disable-mouse
(require-pkg 'disable-mouse t)
(global-disable-mouse-mode)

;; dockerfile-mode
(require-pkg 'dockerfile-mode t)

;; dumb-jump
(require-pkg 'dumb-jump t)
(dumb-jump-mode)

;; ebal
(require-pkg 'ebal t)
(setq-default ebal-operation-mode 'stack)
(global-set-key (kbd "C-c e") 'ebal-execute)

;; emojify
(require-pkg 'emojify t)
(add-hook 'after-init-hook #'global-emojify-mode)
(global-set-key (kbd "C-x e") 'emojify-insert-emoji)

;; exec-path-from-shell
(require-pkg 'exec-path-from-shell t)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")

;; flycheck
(require-pkg 'flycheck t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))
(flycheck-define-checker shellcheck
  "A shell script syntax and style checker using Shellcheck."
  :command ("shellcheck" "-f" "checkstyle"
            "-s" (eval (symbol-name sh-shell))
            source)
  :modes sh-mode
  :error-parser flycheck-parse-checkstyle)
(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'sh-zsh '(warning . shellcheck))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; flycheck-flow
(require-pkg 'flycheck-flow t)
(add-hook 'javascript-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))

;; flycheck-rust
(require-pkg 'flycheck-rust t)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; git-gutter-fringe
(require-pkg 'git-gutter-fringe t)
(global-git-gutter-mode t)

;; golden-ratio
(require-pkg 'golden-ratio t)
(golden-ratio-mode 1)

;; gotham-theme
(require-pkg 'gotham-theme)
(load-theme 'gotham t)

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
(global-set-key (kbd "C-x C-a") 'counsel-rg)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; json-mode
(require-pkg 'json-mode t)

;; magit
(require-pkg 'magit t)
(global-set-key (kbd "C-x g") 'magit-status)

;; markdown-mode
(require-pkg 'markdown-mode t)
(add-to-list 'auto-mode-alist '("\\.text\\'"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; midnight
(require-pkg 'midnight t)

;; multiple-cursors
(require-pkg 'multiple-cursors t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)

;; multi-term
(require-pkg 'multi-term t)
(setq-default multi-term-program "/bin/zsh")

;; php-mode
(require-pkg 'php-mode)

;; racer
(require-pkg 'racer t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq-default company-tooltip-align-annotations t)

;; rainbow-delimiters
(require-pkg 'rainbow-delimiters t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; rust-mode
(require-pkg 'rust-mode t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq-default rust-format-on-save t)

;; smartparens
(require-pkg 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; smart-mode-line
(require-pkg 'smart-mode-line t)
(setq-default sml/no-confirm-load-theme t)
(setq-default sml/theme 'respectful)
(sml/setup)

;;toml-mode
(require-pkg 'toml-mode t)

;; undo-tree
(require-pkg 'undo-tree t)
(diminish 'undo-tree-mode "UT")
(global-undo-tree-mode)

;; web-mode
(require-pkg 'web-mode t)
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq-default web-mode-content-types-alist
      '(("jsx" . "\\.jsx?$")))
(setq-default web-mode-enable-auto-pairing t)
(setq-default web-mode-enable-css-colorization t)
(setq-default web-mode-enable-current-element-highlight t)
(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))
(defun web-mode-hook ()
  "Web mode hooks."
  (setq-default web-mode-attr-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-indent-style 2)
  (setq-default web-mode-markup-indent-offset 2))
(add-hook 'web-mode-hook  'web-mode-hook)
(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

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
