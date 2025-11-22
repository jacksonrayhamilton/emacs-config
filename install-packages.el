;; -*- lexical-binding: t; -*-

;; Run like:
;;
;;   $ emacs --script install-packages.el

(require 'package)
(require 'package-vc)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(setq package-selected-packages
      '(amx auto-complete company eat expand-region flx-ido flycheck
        git-modes ido-completing-read+ js-ts-defs magit magit-ido
        markdown-mode projectile restart-emacs tide yaml-mode yasnippet
        zerodark-theme))

(setq package-vc-selected-packages
      ;; https://github.com/stevemolitor/claude-code.el hangs
      '((claude-code :url "https://github.com/jacksonrayhamilton/claude-code.el")))

(package-install-selected-packages t)
(package-vc-install-selected-packages)
