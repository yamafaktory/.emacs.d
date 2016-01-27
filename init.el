;;; init.el --- yamafaktory

;;; Copyright (c) 2016 Davy Duperron - BSD-3 license

;;; Commentary:
;;; Emacs initialization file.

;;; Code:

;; Desktop saving.
(setq desktop-restore-eager 5
      desktop-save (quote if-exists))
(desktop-save-mode t)

;; Remove splash screen.
(setq inhibit-startup-message t)

;; Remove unwanted UI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Go fullscreen.
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; No blinking cursor.
(blink-cursor-mode -1)

;; No tooltip.
(tooltip-mode -1)

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
(setq dotfiles-dir (file-name-directory
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
  "Only install a package if needed."
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
            (setq sgml-basic-offset 4)))
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
(setq default-fill-column 79)

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
(add-to-list 'default-frame-alist '(font . "Fira Mono-14"))
(set-face-attribute 'default t :font "Fira Mono-14")

;; Highlight current line.
(global-hl-line-mode t)

;; Load other packages.
(require 'packages)

;; Load custom functions.
(require 'utilities)

;;; init.el ends here
