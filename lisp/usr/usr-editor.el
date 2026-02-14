;;; usr-editor.el --- Editor behavior configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Core editor functionality:
;; - which-key (keybinding hints)
;; - Parenthesis matching
;; - Window configuration tracking (winner-mode)
;; - Undo/redo functionality

;;; Code:

(use-package which-key
 :ensure nil ; builtin
 :commands which-key-mode
 :hook (after-init . which-key-mode)
 :diminish which-key-mode
 :custom
 (which-key-idle-delay 1.5)
 (which-key-idle-secondary-delay 0.25)
 (which-key-add-column-padding 1)
 (which-key-max-description-length 40))

;; Paren match highlighting
(add-hook 'after-init-hook #'show-paren-mode)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(add-hook 'after-init-hook #'winner-mode)

;; The undo-fu package is a lightweight wrapper around Emacs' built-in undo
;; system, providing more convenient undo/redo functionality.
(use-package undo-fu
 :ensure t
 :commands (undo-fu-only-undo
            undo-fu-only-redo
            undo-fu-only-redo-all
            undo-fu-disable-checkpoint)
 :config
 (global-unset-key (kbd "C-z"))
 (global-set-key (kbd "C-z") 'undo-fu-only-undo)
 (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; The undo-fu-session package complements undo-fu by enabling the saving
;; and restoration of undo history across Emacs sessions, even after restarting.
(use-package undo-fu-session
 :ensure t
 :commands undo-fu-session-global-mode
 :hook (after-init . undo-fu-session-global-mode))

(provide 'usr-editor)
;;; usr-editor.el ends here
