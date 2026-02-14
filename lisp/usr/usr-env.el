;;; usr-env.el --- Environment and package settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic environment configuration:
;; - Package upgrade settings
;; - Indentation settings for various languages

;;; Code:

;; Allow Emacs to upgrade built-in packages, such as Org mode
(setq package-install-upgrade-built-in t)

;; Indentation: always use spaces, never tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Lisp: fixed 1-space indent aligns plist keys like :url/:rev
(setq lisp-indent-offset 1)

;; Python: 4 spaces
(setq python-indent-offset 4)

;; Shell: 4 spaces
(setq sh-basic-offset 4)
(setq sh-indentation 4)

;; JSON: 4 spaces
(setq js-indent-level 4)
(setq json-ts-mode-indent-offset 4)

;; YAML: 4 spaces
(setq yaml-indent-offset 4)

(provide 'usr-env)
;;; usr-env.el ends here
