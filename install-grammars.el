;; -*- lexical-binding: t; -*-

;; Run like:
;;
;;   $ emacs --script install-grammars.el

(require 'treesit)

;; (Compatible with Emacs 30)
(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.23.2" "typescript/src")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
