;;; usr-utils.el --- Utility packages configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous utility packages:
;; - TRAMP (remote file access)
;; - stripspace (trailing whitespace cleanup)
;; - ultra-scroll (smooth scrolling)
;; - nyan-mode (progress indicator)
;; - diminish (hide minor modes)
;; - Mouse configuration
;; - difftastic (better diffs)
;; - focus-mode (distraction-free editing)
;; - daemons (system services management)
;; - dired-rsync (async file copy)
;; - eshell-atuin (shell history)
;; - esh-autosuggest (command suggestions)
;; - YAML/async packages

;;; Code:

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
 :ensure t
 :commands stripspace-local-mode
 ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
 :hook ((prog-mode . stripspace-local-mode)
        (text-mode . stripspace-local-mode)
        (conf-mode . stripspace-local-mode))
 :diminish stripspace-local-mode
 :custom
 ;; The `stripspace-only-if-initially-clean' option:
 ;; - nil to always delete trailing whitespace.
 ;; - Non-nil to only delete whitespace when the buffer is clean initially.
 ;; (The initial cleanliness check is performed when `stripspace-local-mode'
 ;; is enabled.)
 (stripspace-only-if-initially-clean nil)
 ;; Enabling `stripspace-restore-column' preserves the cursor's column position
 ;; even after stripping spaces. This is useful in scenarios where you add
 ;; extra spaces and then save the file. Although the spaces are removed in the
 ;; saved file, the cursor remains in the same position, ensuring a consistent
 ;; editing experience without affecting cursor placement.
 (stripspace-restore-column t))

;; Smooth scroll ftw
(use-package ultra-scroll
 :ensure t
 :vc (:url "https://github.com/jdtsmith/ultra-scroll")
 :init
 (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
  scroll-margin 0)        ; important: scroll-margin>0 not yet supported
 :config
 (ultra-scroll-mode 1))

;; TRAMP configuration
(use-package tramp
 :ensure nil
 :custom
 (tramp-verbose 1)
 (remote-file-name-inhibit-locks t)
 (remote-file-name-inhibit-cache nil)  ; Cache remote file attributes
 (tramp-completion-reread-directory-timeout nil)  ; Never re-read directories
 (tramp-use-scp-direct-remote-copying t)
 (remote-file-name-inhibit-auto-save-visited t)
 :config
 ;; Skip VC on remote files (major speedup)
 (when (and (boundp 'vc-ignore-dir-regexp) vc-ignore-dir-regexp)
  (setq vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
    vc-ignore-dir-regexp
    tramp-file-name-regexp)))
 (connection-local-set-profile-variables
  'remote-direct-async-process
  '((tramp-direct-async-process . t)))
 (connection-local-set-profiles
  '(:application tramp :protocol "scp")
  'remote-direct-async-process)
 (with-eval-after-load 'compile
  (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; Salt inventory host selector
(use-package yaml
 :ensure t)

(use-package async
 :ensure t)

(use-package nyan-mode
 :ensure t
 :hook (after-init . nyan-mode))

(use-package diminish
 :config
 (diminish 'abbrev-mode)
 (diminish 'flyspell-mode)
 (diminish 'flyspell-prog-mode)
 (diminish 'eldoc-mode))

;; Enable horizontal scroll via touchpad (reversed direction)
;; Use pixel-precision scrolling to match vertical smoothness
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)  ; Reversed: wheel-left scrolls right
(global-set-key (kbd "<wheel-left>")
 (lambda () (interactive) (scroll-right 1)))
(global-set-key (kbd "<wheel-right>")
 (lambda () (interactive) (scroll-left 1)))

;; Better diffs
(use-package difftastic
 :defer t
 :vc (:url "https://github.com/pkryger/difftastic.el.git"
      :rev :newest))

(use-package focus-mode
 :vc (:url "https://git.sr.ht/~mgmarlow/focus-mode"
      :rev :newest))

(use-package daemons
 :ensure t)

(use-package dired-rsync
 :ensure t
 :bind (:map dired-mode-map
        ("C-c C-r" . dired-rsync)))

(use-package dired-rsync-transient
 :ensure t)

(use-package eshell-atuin
 :vc (:url "https://github.com/SqrtMinusOne/eshell-atuin"
      :rev :newest)
 :after eshell
 :config
 (eshell-atuin-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  ;; If you have use-package-hook-name-suffix set to nil, uncomment and use the
  ;; line below instead:
  ;; :hook (eshell-mode-hook . esh-autosuggest-mode)
  :ensure t)

(provide 'usr-utils)
;;; usr-utils.el ends here
