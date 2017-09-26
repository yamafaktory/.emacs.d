;;; init.el --- yamafaktory

;;; Copyright (c) 2017 Davy Duperron - BSD-3 license

;;; Commentary:
;;; Emacs initialization file.

;;; Code:

;; Go fullscreen.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Desktop saving.
(setq-default desktop-restore-eager 5
              desktop-save (quote if-exists))
(desktop-save-mode t)

;; Remove splash screen.
(setq inhibit-startup-message t)

;; Remove unwanted UI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No blinking cursor.
(blink-cursor-mode -1)

;; No tooltip.
(tooltip-mode -1)

;; Disable arrow and delete keys.
(global-set-key (kbd "<up>") 'ignore)
(global-set-key (kbd "<down>") 'ignore)
(global-set-key (kbd "<left>") 'ignore)
(global-set-key (kbd "<right>") 'ignore)
(global-set-key (kbd "<delete>") 'ignore)

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Save buffers before compiling.
(setq compilation-ask-about-save nil)

;; Disable lock files.
(setq create-lockfiles nil)

;; Add other startup files to load path.
(setq-default dotfiles-dir (file-name-directory
                            (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "yamafaktory"))

;; Write backup files to own directory.
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "backup")))))

;; Packages sources.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-pkg (pkg &optional req)
  "Install a package PKG if and only if needed, require it if REQ is true."
  (unless (package-installed-p pkg)
    (package-install pkg))
  (if req (require pkg)))

;; No tabs.
(set-default 'indent-tabs-mode nil)

;; Indentation settings.
(setq-default tab-width 2)
(add-hook 'css-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))
(add-hook 'html-mode-hook
          (lambda ()
            (setq sgml-basic-offset 2)))
(add-hook 'json-mode-hook
          (lambda ()
            (setq js-indent-level 2)))
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

;; Line number.
(line-number-mode t)

;; Column number.
(column-number-mode t)

;; File size.
(size-indication-mode t)

;; No cursor in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

;; No double space at the end of sentences.
(setq sentence-end-double-space nil)

;; Default column size.
(setq fill-column 79)

;; Newline at file end's.
(setq require-final-newline t)

;; Remove trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Soft wrap lines.
(global-visual-line-mode t)

;; Disable ring bell.
(setq ring-bell-function 'ignore)

;; Auto refresh buffers.
(global-auto-revert-mode t)

;; Simplified yep nope.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Typed text replaces selection.
(delete-selection-mode t)

;; Font face and size.
;; https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(add-to-list 'default-frame-alist '(font . "Fira Code-18"))
(set-face-attribute 'default t :font "Fira Code-18")
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; Highlight current line.
(global-hl-line-mode t)

;; Load custom functions first.
(require 'utilities)

;; Load other packages.
(require 'packages)

;;; init.el ends here
