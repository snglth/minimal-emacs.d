;;; usr-files.el --- File management configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; File handling and persistence:
;; - Dired configuration
;; - Backup settings
;; - Auto-revert mode
;; - Recent files tracking
;; - Minibuffer history persistence
;; - Cursor position persistence

;;; Code:

;; Constrain vertical cursor movement to lines within the buffer
(setq dired-movement-style 'bounded-files)

;; Dired buffers: Automatically hide file details (permissions, size,
;; modification date, etc.) and all the files in the `dired-omit-files' regular
;; expression for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Hide files from dired
(setq dired-omit-files (concat "\\`[.]\\'"
                        "\\|\\(?:\\.js\\)?\\.meta\\'"
                        "\\|\\.\\(?:elc|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'"
                        "\\|^\\.DS_Store\\'"
                        "\\|^\\.\\(?:svn\\|git\\)\\'"
                        "\\|^\\.ccls-cache\\'"
                        "\\|^__pycache__\\'"
                        "\\|^\\.project\\(?:ile\\)?\\'"
                        "\\|^flycheck_.*"
                        "\\|^flymake_.*"))
(add-hook 'dired-mode-hook #'dired-omit-mode)

;; dired: Group directories first
(with-eval-after-load 'dired
 (let ((args "--group-directories-first -ahlv"))
  (when (or (eq system-type 'darwin) (eq system-type 'berkeley-unix))
   (if-let* ((gls (executable-find "gls")))
    (setq insert-directory-program gls)
    (setq args nil)))
  (when args
   (setq dired-listing-switches args))))

;; Enables visual indication of minibuffer recursion depth after initialization.
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

;; Enabled backups save your changes to a file intermittently
(setq make-backup-files t)
(setq vc-make-backup-files t)
(setq kept-old-versions 10)
(setq kept-new-versions 10)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
 :ensure nil
 :commands (auto-revert-mode global-auto-revert-mode)
 :hook
 (after-init . global-auto-revert-mode)
 :custom
 (auto-revert-interval 3)
 (auto-revert-remote-files nil)
 (auto-revert-use-notify t)
 (auto-revert-avoid-polling nil)
 (auto-revert-verbose t))

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
 :ensure nil
 :commands (recentf-mode recentf-cleanup)
 :hook
 (after-init . recentf-mode)
 :custom
 (recentf-auto-cleanup (if (daemonp) 300 'never))
 (recentf-exclude
  (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
   "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
   "\\.7z$" "\\.rar$"
   "COMMIT_EDITMSG\\'"
   "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
   "-autoloads\\.el$" "autoload\\.el$"))
 :config
 ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
 ;; `recentf-save-list', allowing stale entries to be removed before the list
 ;; is saved by `recentf-save-list', which is automatically added to
 ;; `kill-emacs-hook' by `recentf-mode'.
 (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
 :ensure nil
 :commands (savehist-mode savehist-save)
 :hook
 (after-init . savehist-mode)
 :custom
 (savehist-autosave-interval 600)
 (savehist-additional-variables
  '(kill-ring                        ; clipboard
    register-alist                   ; macros
    mark-ring global-mark-ring       ; marks
    search-ring regexp-search-ring)))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
 :ensure nil
 :commands (save-place-mode save-place-local-mode)
 :hook
 (after-init . save-place-mode)
 :custom
 (save-place-limit 400))

(provide 'usr-files)
;;; usr-files.el ends here
