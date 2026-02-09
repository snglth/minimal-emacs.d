;;; post-init.el --- Post-init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Loads modular configuration from lisp/usr/ directory.
;;
;; This file has been refactored into focused modules for better maintainability.
;; Each module handles a specific functional area (UI, completion, git, etc.).
;;
;; Module load order is critical - performance and environment setup must load
;; first, followed by UI and core features, then specialized tools.

;;; Code:

;; Ensure lisp/ and lisp/usr/ are in load-path for custom packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/usr" user-emacs-directory))

;; Core infrastructure (must load first)
(require 'usr-performance)  ; compile-angel, exec-path-from-shell, direnv
(require 'usr-env)          ; Package settings, indentation

;; Visual interface
(require 'usr-ui)           ; Theme, font, line numbers, tooltips
(require 'usr-editor)       ; which-key, parens, winner-mode, undo-fu

;; File management
(require 'usr-files)        ; Dired, recentf, savehist, saveplace, autorevert

;; Completion framework
(require 'usr-completion)   ; Corfu, Cape, Vertico, Orderless, Marginalia, Embark, Consult

;; Navigation
(require 'usr-transient)    ; Transient menu system with SPC leader key

;; Development tools
(require 'usr-programming)  ; Flycheck, treesit-auto, eglot, outline
(require 'usr-terminal)     ; Eat, MisTTY, eshell

;; Version control
(require 'usr-git)          ; Magit, git-gutter, forge, pr-review

;; Modal editing
(require 'usr-evil)         ; Evil, evil-collection

;; AI tools
(require 'usr-ai)           ; GPTel, claude-code-ide, agent-shell, prism

;; Language support
(require 'usr-languages)    ; Salt, paredit, org-mode, nix-mode

;; Utilities
(require 'usr-utils)        ; TRAMP, stripspace, ultra-scroll, misc tools

(provide 'post-init)
;;; post-init.el ends here
