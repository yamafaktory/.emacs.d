;;; init.el --- yamafaktory

;; Save session
(desktop-save-mode 1)

;; Remove splash screen
(setq inhibit-startup-message t)

;; Remove unwanted UI elements
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No blinking cursor
(blink-cursor-mode -1)

;; Save session
(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;; No tooltip
(tooltip-mode -1)

;; UTF-8
(set-terminal-coding-system 'utf-8)
(set keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Save buffers before compiling
(setq compilation-ask-about-save nil)

;; Add other startup files to load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "yamafaktory"))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "backup")))))

;; Packages sources
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/package/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-pkg (pkg)
  "Only install a package if needed"
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; No tabs
(set-default 'indent-tabs-mode nil)

;; Indent
(setq-default tab-width 2)

;; Line numbers
(global-linum-mode t)

;; No double space at the end of sentences
(setq sentence-end-double-space nil)

;; Default column size
(setq default-fill-column 79)

;; No newline at file end's
(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; Disable ring bell
(setq ring-bell-function 'ignore)

;; Desktop saving
(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;; Load other packages
(require 'packages)
