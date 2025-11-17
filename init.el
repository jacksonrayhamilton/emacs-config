;; -*- lexical-binding: t; -*-

;;; Theme and Appearance

(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defconst my-default-font
  (cond
   ((eq system-type 'darwin)
    "monaco 16")
   ((eq system-type 'windows-nt)
    "consolas 16")
   (t
    "monospace 16")))

(set-frame-font my-default-font)
(add-to-list 'default-frame-alist `(font . ,my-default-font))

(load-theme 'zerodark t)
(zerodark-setup-modeline-format)

;;; Standardization

(cua-mode)

(keymap-global-set "C-a" #'mark-whole-buffer)
(keymap-global-set "C-o" #'find-file)
(keymap-global-set "C-b" #'switch-to-buffer)
(keymap-global-set "C-s" #'save-buffer)
(keymap-global-set "C-w" #'kill-current-buffer)
(keymap-global-set "C-f" #'isearch-forward)
(keymap-global-set "C-r" #'query-replace)

;;; Navigation

(keymap-global-set "C-/" #'dired-jump)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)
(amx-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights
(setq gc-cons-threshold 20000000) ; recommended for better GC with flx

;;; Text Selection

(keymap-global-set "C-=" #'er/expand-region)
(keymap-global-set "C-+" #'er/contract-region)

;;; Screen Splitting

(keymap-global-set "C-0" #'delete-window)
(keymap-global-set "C-1" #'delete-other-windows)
(keymap-global-set "C-2" #'split-window-below)
(keymap-global-set "C-3" #'split-window-right)

;;; Open Emacs Config File

(defun my-find-configuration ()
  "Open the Emacs configuration file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(keymap-global-set "<f5>" #'my-find-configuration)

;;; Restart Emacs

(require 'restart-emacs) ; load 3rd-party version

(defun my-restart ()
  "Prompt whether to restart Emacs."
  (interactive)
  (when (y-or-n-p "Restart?")
    (restart-emacs)))

(keymap-global-set "<f6>" #'my-restart)

;;; Git Interface

(require 'magit)
(require 'magit-ido)

(keymap-global-set "<f7>" #'magit-status)
(setq magit-completing-read-function #'magit-ido-completing-read)

;;; Project Management

(projectile-mode)

(keymap-global-set "<f8>" #'projectile-commander)

;;; AI Assistant

(require 'claude-code)

(setq claude-code-toggle-auto-select t)
(setq claude-code-eat-read-only-mode-cursor-type '(bar nil nil))

(keymap-global-set "<f9>" #'claude-code-transient)
