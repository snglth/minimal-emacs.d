;;; usr-ui.el --- User interface configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual interface settings:
;; - Line and column numbers
;; - Tree-sitter font lock
;; - Window dividers
;; - Tooltips
;; - Buffer name uniquification
;; - Theme and font
;; - Platform-specific fixes

;;; Code:

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

;; Display of line numbers in the buffer:
(setq-default display-line-numbers-type t)
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
 (add-hook hook #'display-line-numbers-mode))

;; Set the maximum level of syntax highlighting for Tree-sitter modes
(setq treesit-font-lock-level 4)

(use-package uniquify
 :ensure nil
 :custom
 (uniquify-buffer-name-style 'reverse)
 (uniquify-separator "•")
 (uniquify-after-kill-buffer-p t))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; When tooltip-mode is enabled, certain UI elements (e.g., help text,
;; mouse-hover hints) will appear as native system tooltips (pop-up windows),
;; rather than as echo area messages. This is useful in graphical Emacs sessions
;; where tooltips can appear near the cursor.
(setq tooltip-hide-delay 20)    ; Time in seconds before a tooltip disappears (default: 10)
(setq tooltip-delay 0.4)        ; Delay before showing a tooltip after mouse hover (default: 0.7)
(setq tooltip-short-delay 0.08) ; Delay before showing a short tooltip (Default: 0.1)
(tooltip-mode 1)

;; ls does not support --dired flag on mac
(when (string= system-type "darwin")
 (setq dired-use-ls-dired nil))

;; Setup secrets lookup
(use-package auth-source
 :ensure nil
 :config
 (when (string= system-type "darwin") ; Lookup keychain for secrets on mac
  (add-to-list 'auth-sources 'macos-keychain-internet)
  (add-to-list 'auth-sources 'macos-keychain-generic)))

;; Enable alabaster theme
;; Trust all themes (no confirmation prompts)
(setq custom-safe-themes t)

;; Disable any themes enabled before this point
(mapc #'disable-theme custom-enabled-themes)

;; Load alabaster theme at startup
(use-package alabaster-themes
 :ensure t
 :demand t
 :config
 (load-theme 'alabaster-themes-dark t))

;; Set font
(set-face-attribute 'default nil
 :height 160 :weight 'normal :family "Iosevka Nerd Font")

(provide 'usr-ui)
;;; usr-ui.el ends here
