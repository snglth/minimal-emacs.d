;;; usr-performance.el --- Performance and environment setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Critical infrastructure that must load first:
;; - Native compilation (compile-angel)
;; - PATH/environment setup (cached exec-path-from-shell)
;; - Project environment integration (direnv)

;;; Code:

;; Disable package signature verification
(setq package-check-signature nil)

(use-package compile-angel
 :ensure t
 :custom
 (compile-angel-verbose nil)
 :diminish compile-angel-on-load-mode
 :config
 (push "/init.el" compile-angel-excluded-files)
 (push "/early-init.el" compile-angel-excluded-files)
 (push "/pre-init.el" compile-angel-excluded-files)
 (push "/post-init.el" compile-angel-excluded-files)
 (push "/pre-early-init.el" compile-angel-excluded-files)
 (push "/post-early-init.el" compile-angel-excluded-files)
 (compile-angel-on-load-mode 1))

;; Defer compile-angel activation until after startup so the init
;; module + package load cascade runs without compile-check overhead.
;; Also skip the expensive locate-file scan for already-loaded features.
(add-hook 'emacs-startup-hook
 (lambda ()
  (require 'compile-angel)
  ;; compile-angel's :before advice on `require' calls locate-file
  ;; (scanning all of load-path) for EVERY require, even for features
  ;; already loaded. This adds thousands of stat() syscalls per file-open.
  ;; Short-circuit for already-loaded features.
  (defun my/compile-angel-skip-loaded-a (orig-fn feature &rest args)
    "Skip compile-angel file lookup for already-loaded features."
    (unless (featurep feature)
      (apply orig-fn feature args)))
  (advice-add 'compile-angel--advice-before-require
              :around #'my/compile-angel-skip-loaded-a)))

;; --- Cached exec-path-from-shell ---
;; Instead of spawning a zsh subprocess every startup, cache env vars
;; to a file and reload from cache. Refresh when cache is >24h old
;; or via M-x my/env-cache-refresh.
(defvar my/env-cache-file
  (expand-file-name "env-cache.el" user-emacs-directory)
  "File to cache environment variables from the login shell.")

(defvar my/env-cache-variables '("PATH" "MANPATH" "SSH_AUTH_SOCK")
  "Environment variables to cache from the login shell.")

(defvar my/env-cache-max-age (* 24 60 60)
  "Maximum age of env cache in seconds before auto-refresh (default 24h).")

(defun my/env-cache-stale-p ()
  "Return non-nil if the env cache file is missing or older than `my/env-cache-max-age'."
  (or (not (file-exists-p my/env-cache-file))
      (> (float-time (time-subtract (current-time)
                                    (file-attribute-modification-time
                                     (file-attributes my/env-cache-file))))
         my/env-cache-max-age)))

(defun my/env-cache-save ()
  "Harvest env vars from login shell and write to cache file."
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (dolist (var my/env-cache-variables)
    (unless (string= var "PATH")  ; PATH already handled by initialize
      (exec-path-from-shell-copy-env var)))
  (with-temp-file my/env-cache-file
    (insert ";;; env-cache.el --- auto-generated, do not edit -*- lexical-binding: t; -*-\n")
    (dolist (var my/env-cache-variables)
      (insert (format "(setenv %S %S)\n" var (getenv var))))
    (insert (format "(setq exec-path '%S)\n" exec-path))
    (insert ";;; env-cache.el ends here\n"))
  (message "Environment cache written to %s" my/env-cache-file))

(defun my/env-cache-load ()
  "Load cached env vars, refreshing if stale."
  (if (my/env-cache-stale-p)
      (my/env-cache-save)
    (load my/env-cache-file nil 'nomessage)))

(defun my/env-cache-refresh ()
  "Force refresh the environment cache from the login shell."
  (interactive)
  (my/env-cache-save)
  (message "Environment cache refreshed"))

;; exec-path-from-shell is only needed for the cache save path
(use-package exec-path-from-shell
 :ensure t
 :if (memq window-system '(mac ns x))
 :defer t)

;; Load cached env on startup (or generate cache on first run)
(when (memq window-system '(mac ns x))
  (my/env-cache-load))

;; Use direnv — defer the synchronous `direnv export json` call
;; to an idle timer so it never blocks file opening or commands.
(use-package direnv
 :hook (after-init . direnv-mode)
 :config
 (defvar my/direnv--pending-timer nil
   "Idle timer for deferred direnv update.")
 (defun my/direnv--deferred-update-a (orig-fn)
   "Advice to defer `direnv--maybe-update-environment' to idle time."
   (when (timerp my/direnv--pending-timer)
     (cancel-timer my/direnv--pending-timer))
   (setq my/direnv--pending-timer
         (run-with-idle-timer 0.5 nil
          (lambda ()
            (setq my/direnv--pending-timer nil)
            (funcall orig-fn)))))
 (advice-add 'direnv--maybe-update-environment
             :around #'my/direnv--deferred-update-a))

(provide 'usr-performance)
;;; usr-performance.el ends here
