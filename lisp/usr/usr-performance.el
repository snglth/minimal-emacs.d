;;; usr-performance.el --- Performance and environment setup -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Critical infrastructure that must load first:
;; - Native compilation (compile-angel)
;; - PATH/environment setup (exec-path-from-shell)
;; - Project environment integration (direnv)

;;; Code:

;; Disable package signature verification
(setq package-check-signature nil)

(use-package compile-angel
 :demand t
 :ensure t
 :custom
 ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
 ;; Drawback: The minibuffer will not display compile-angel's actions.
 (compile-angel-verbose t)
 :diminish compile-angel-on-load-mode
 :config
 ;; The following directive prevents compile-angel from compiling your init
 ;; files. If you choose to remove this push to `compile-angel-excluded-files'
 ;; and compile your pre/post-init files, ensure you understand the
 ;; implications and thoroughly test your code. For example, if you're using
 ;; the `use-package' macro, you'll need to explicitly add:
 ;; (eval-when-compile (require 'use-package))
 ;; at the top of your init file.
 (push "/init.el" compile-angel-excluded-files)
 (push "/early-init.el" compile-angel-excluded-files)
 (push "/pre-init.el" compile-angel-excluded-files)
 (push "/post-init.el" compile-angel-excluded-files)
 (push "/pre-early-init.el" compile-angel-excluded-files)
 (push "/post-early-init.el" compile-angel-excluded-files)
 ;; A local mode that compiles .el files whenever the user saves them.
 ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
 ;; A global mode that compiles .el files prior to loading them via `load' or
 ;; `require'. Additionally, it compiles all packages that were loaded before
 ;; the mode `compile-angel-on-load-mode' was activated.
 (compile-angel-on-load-mode 1))

;; Inherit PATH from login shell (zsh)
(use-package exec-path-from-shell
 :ensure t
 :if (memq window-system '(mac ns x))
 :config
 (exec-path-from-shell-initialize)
 (exec-path-from-shell-copy-env "MANPATH")
 (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Use direnv
(use-package direnv
 :config
 (direnv-mode))

(provide 'usr-performance)
;;; usr-performance.el ends here
