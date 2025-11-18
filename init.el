;; -*- lexical-binding: t; -*-

;;; Packages

(require 'package)
(require 'package-vc)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; List of packages you want installed
(setq package-selected-packages
      '(amx auto-complete company eat expand-region flx-ido flycheck
        ido-completing-read+ js-ts-defs magit magit-ido markdown-mode
        projectile restart-emacs tide yasnippet))

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

;;; Grammars

(require 'treesit)

;; (Compatible with Emacs 30)
(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")))

(defun my-treesit-install-all-languages ()
  "Install all missing Tree-sitter grammars from `treesit-language-source-alist'."
  (interactive)
  (dolist (lang-source treesit-language-source-alist)
    (let* ((lang (car lang-source)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

(add-hook 'emacs-startup-hook #'my-treesit-install-all-languages)

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

(defun my-ido-select-current-directory ()
  "Select the current directory (.) and exit."
  (interactive)
  (setq ido-exit 'done)
  (setq ido-text ".")
  (exit-minibuffer))

(defun my-ido-setup-hook ()
  (define-key ido-completion-map (kbd "C-o") #'my-ido-select-current-directory))

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

;;; JavaScript

(require 'flycheck)
(require 'js)

;; Enable tree-sitter for JavaScript
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))

(defun my-javascript-mode-hook ()
  (flycheck-mode))

(add-hook 'js-ts-mode-hook #'my-javascript-mode-hook)

;; Jump to definition
(define-key js-ts-mode-map (kbd "M-.") #'js-ts-defs-jump-to-definition)

;;; TypeScript

(require 'tide)
(require 'company)

;; Enable tree-sitter for TypeScript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(defun my-setup-tide ()
  (tide-setup)
  (flycheck-mode)
  (company-mode))

(add-hook 'typescript-ts-mode-hook #'my-setup-tide)
(add-hook 'tsx-ts-mode-hook #'my-setup-tide)

;; Check TypeScript, then ESLint
(flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
(flycheck-add-next-checker 'tsx-tide 'javascript-eslint)

;;; Auto Complete

(require 'auto-complete)

(global-auto-complete-mode t)
(add-to-list 'ac-modes 'js-ts-mode)
(setq ac-ignore-case nil)

;;; Snippets

(require 'yasnippet)

;; Store snippets in your config directory
(setq yas-snippet-dirs `(,(concat user-emacs-directory "Snippets")))

(yas-reload-all)

;; Use Ctrl+Tab to expand snippets (avoiding conflicts with other tab uses)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") #'yas-insert-snippet)

;; Enable snippets in programming modes
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;; Markdown

(require 'markdown-mode)

(add-hook 'markdown-mode-hook #'visual-line-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; Set the markdown preview command
(setq markdown-command "marked")

;; Bind Ctrl+P to preview
(define-key markdown-mode-map (kbd "C-p") #'markdown-preview)

;;; Org Mode

(require 'org)
(require 'ox) ; org export

(add-hook 'org-mode-hook #'visual-line-mode)

(defun my-org-preview ()
  (interactive)
  (browse-url-of-buffer
   (let ((org-export-show-temporary-export-buffer nil))
     (org-html-export-as-html))))

(define-key org-mode-map (kbd "C-p") #'my-org-preview)

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
