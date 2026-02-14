;;; usr-languages.el --- Language-specific configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Programming language support:
;; - Salt mode
;; - Paredit (Lisp structural editing)
;; - Elisp tools (page-break-lines, elisp-refs)
;; - Org mode and ob-d2 (diagrams)
;; - Nix mode

;;; Code:

;; Salt niceties
(use-package salt-mode
 :ensure t
 :mode "\\.j2\\'"
 :config
 (add-hook 'salt-mode-hook
  (lambda ()
   (flyspell-mode 1))))

;; Prevent parenthesis imbalance
(use-package paredit
 :ensure t
 :commands paredit-mode
 :diminish paredit-mode
 :hook
 (emacs-lisp-mode . paredit-mode)
 :config
 (define-key paredit-mode-map (kbd "RET") nil))

;; For paredit+Evil mode users: enhances paredit with Evil mode compatibility
(use-package enhanced-evil-paredit
 :ensure t
 :commands enhanced-evil-paredit-mode
 :diminish enhanced-evil-paredit-mode
 :hook
 (paredit-mode . enhanced-evil-paredit-mode))

;; Displays visible indicators for page breaks
(use-package page-break-lines
 :ensure t
 :commands (page-break-lines-mode
            global-page-break-lines-mode)
 :diminish page-break-lines-mode
 :hook
 (emacs-lisp-mode . page-break-lines-mode))

;; Provides functions to find references to functions, macros, variables,
;; special forms, and symbols in Emacs Lisp
(use-package elisp-refs
 :ensure t
 :commands (elisp-refs-function
            elisp-refs-macro
            elisp-refs-variable
            elisp-refs-special
            elisp-refs-symbol))

;; Org mode is a major mode designed for organizing notes, planning, task
;; management, and authoring documents using plain text with a simple and
;; expressive markup syntax. It supports hierarchical outlines, TODO lists,
;; scheduling, deadlines, time tracking, and exporting to multiple formats
;; including HTML, LaTeX, PDF, and Markdown.

;; Diagramming language
(use-package ob-d2
 :load-path "lisp"
 :custom
 ;; Use monospace font
 (ob-d2-default-flags "--font-regular=/System/Library/Fonts/Menlo.ttc")
 ;; Alabaster theme colors for D2 diagrams
 ;; D2 uses N7 for background, N1 for text (reversed from intuition)
 (ob-d2-vars
  '(("d2-config"
     . (("theme-id" . 200)       ; force dark theme
        ("pad" . 10)
        ("theme-overrides"
         . (("N7" . "transparent") ; bg-main (transparent)
            ("N6" . "#1f2526")    ; bg-alt
            ("N5" . "#293334")    ; bg-active
            ("N4" . "#444444")    ; border
            ("N3" . "#666666")    ; fg-dim
            ("N2" . "#a0a0a0")    ; fg-alt
            ("N1" . "#CECECE")    ; fg-main (text)
            ("B1" . "#3a3a3a")    ; container bg darkest
            ("B2" . "#4a4a4a")    ; container bg dark
            ("B3" . "#5a5a5a")    ; container bg mid
            ("B4" . "#6a6a6a")    ; container fill
            ("B5" . "#7a7a7a")    ; container fill light
            ("B6" . "#8a8a8a")    ; container fill lighter
            ("AA2" . "#707070")   ; accent dark
            ("AA4" . "#909090")   ; accent mid
            ("AA5" . "#b0b0b0")   ; accent light
            ("AB4" . "#808080")   ; secondary accent
            ("AB5" . "#a0a0a0"))))))))

;; org itself
(use-package org
 :ensure t
 :after ob-d2
 :commands (org-mode org-version)
 :mode
 ("\\.org\\'" . org-mode)
 :custom
 (org-hide-leading-stars t)
 (org-startup-indented t)
 (org-adapt-indentation nil)
 (org-edit-src-content-indentation 0)
 (org-fontify-done-headline t)
 (org-fontify-todo-headline t)
 (org-fontify-whole-heading-line t)
 (org-fontify-quote-and-verse-blocks t)
 (org-startup-truncated t)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((d2 . t)))
 :config
 (require 'ox-md))

;; Syntax highligting & other stuff for nix files
(use-package nix-mode
 :mode "\\.nix\\'")

(provide 'usr-languages)
;;; usr-languages.el ends here
