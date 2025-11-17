;; -*- lexical-binding: t; -*-

;;; Packages

(require 'package)
(require 'package-vc)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; List of packages you want installed
(setq package-selected-packages
      '(amx eat expand-region flx-ido ido-completing-read+
        magit magit-ido projectile restart-emacs))

;; VC packages (installed from version control)
(setq package-vc-selected-packages
  '((claude-code :url "https://github.com/jacksonrayhamilton/claude-code.el")
    (zerodark-theme :url "https://github.com/jacksonrayhamilton/zerodark-theme"
                    :branch "optional-dependencies")))

;; Refresh and install missing packages at startup
(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(dolist (pkg package-vc-selected-packages)
  (unless (package-installed-p (car pkg))
    (package-vc-install pkg)))

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

(require 'dired)

;; Carry over global remappings to modes
(define-key dired-mode-map (kbd "C-o") #'find-file)
(define-key isearch-mode-map (kbd "C-v") #'isearch-yank-kill)
(define-key isearch-mode-map (kbd "C-f") #'isearch-repeat-forward)

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

(defun my-ido-setup-hook ()
  ;; Escape hatch to open directories via `find-file'
  (define-key ido-completion-map (kbd "C-o") #'ido-fallback-command))

(add-hook 'ido-setup-hook #'my-ido-setup-hook)

;;; Text Selection

(keymap-global-set "C-=" #'er/expand-region)
(keymap-global-set "C-+" #'er/contract-region)

;;; Search

(defun my-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'my-isearch-with-region)

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

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
