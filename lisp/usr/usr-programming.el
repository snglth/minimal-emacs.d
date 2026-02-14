;;; usr-programming.el --- Programming tools configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Development environment setup:
;; - Flycheck (syntax checking)
;; - Tree-sitter (syntax highlighting)
;; - Eglot (LSP client)
;; - Outline (code folding)

;;; Code:

;; Flycheck is a modern on-the-fly syntax checking extension.
(use-package flycheck
 :ensure t
 :commands (flycheck-mode
            flycheck-next-error
            flycheck-previous-error
            flycheck-list-errors
            flycheck-buffer
            flycheck-clear
            flycheck-select-checker
            flycheck-verify-setup
            flycheck-describe-checker
            flycheck-disable-checker)
 :hook (prog-mode . my/flycheck-mode-deferred)
 :custom
 (flycheck-emacs-lisp-load-path 'inherit)
 (flycheck-indication-mode 'left-fringe)
 (flycheck-display-errors-function nil)
 (flycheck-checker-error-threshold 100)
 :config
 (defun my/flycheck-mode-deferred ()
   "Enable flycheck-mode after a short idle delay."
   (run-with-idle-timer 1.0 nil
    (lambda (buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (not flycheck-mode)
                     (derived-mode-p 'prog-mode))
            (flycheck-mode 1)))))
    (current-buffer)))
 ;; Suppress flycheck warnings via Emacs warning system
 (add-to-list 'warning-suppress-types '(flycheck syntax-checker))
 ;; Suppress "Suspicious state" messages from echo area
 (defun flycheck-suppress-suspicious-message-a (orig-fun format-string &rest args)
  "Filter out flycheck Suspicious state messages."
  (unless (and format-string
           (stringp format-string)
           (string-match-p "Suspicious state" format-string))
   (apply orig-fun format-string args)))
 (advice-add 'message :around #'flycheck-suppress-suspicious-message-a)
 ;; Fix python-ruff checker for newer ruff versions
 (flycheck-define-checker python-ruff
  "A Python syntax and style checker using Ruff."
  :command ("ruff" "check"
            "--output-format=concise"
            (option "--stdin-filename" buffer-file-name)
            "-")
  :standard-input t
  :error-patterns
  ((error line-start
    (or "-" (file-name)) ":" line ":" column ": "
    (or "SyntaxError" "invalid-syntax") ": "
    (message (one-or-more not-newline))
    line-end)
   (warning line-start
    (or "-" (file-name)) ":" line ":" column ": "
    (id (one-or-more (any alpha)) (one-or-more digit)) " "
    (message (one-or-more not-newline))
    line-end))
  :modes (python-mode python-ts-mode python-base-mode))
 ;; Use ruff for Python
 (add-hook 'python-base-mode-hook
  (lambda () (setq-local flycheck-checker 'python-ruff)))
 ;; Emacs Lisp: chain checkdoc after byte-compile checker
 (flycheck-add-next-checker 'emacs-lisp '(warning . emacs-lisp-checkdoc))
 ;; Exclude init files from checkdoc (they have special conventions)
 (add-hook 'emacs-lisp-mode-hook
  (lambda ()
   (when (and buffer-file-name
          (string-match-p "\\(?:init\\|early-init\\)\\.el\\'" buffer-file-name))
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))
 ;; Display error list at bottom, max 10 lines
 (add-to-list 'display-buffer-alist
  '("\\*Flycheck errors\\*"
    (display-buffer-reuse-window display-buffer-at-bottom)
    (window-height . 10)
    (reusable-frames . visible)))
 ;; Wider filename column
 (setq flycheck-error-list-format
  `[("File" 30 flycheck-error-list-entry-< :props (face flycheck-error-list-filename))
    ("Line" 5 flycheck-error-list-entry-< :right-align t)
    ("Col" 3 nil :right-align t)
    ("Level" 8 flycheck-error-list-entry-<)
    ("ID" 6 t)
    (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]))

;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
 :ensure t
 :defer 0.1
 :custom
 (treesit-auto-install 'prompt)
 :config
 (treesit-auto-add-to-auto-mode-alist 'all)
 (global-treesit-auto-mode)

 ;; Cache the remap alist so treesit-language-available-p isn't called
 ;; for every grammar on every file open. The alist only changes when
 ;; grammars are installed/removed, which never happens mid-session.
 (defvar my/treesit-auto--remap-cache nil
   "Cached result of `treesit-auto--build-major-mode-remap-alist'.")
 (defun my/treesit-auto--cache-remap-a (orig-fn)
   "Return cached remap alist, building it only on first call."
   (or my/treesit-auto--remap-cache
       (setq my/treesit-auto--remap-cache (funcall orig-fn))))
 (advice-add 'treesit-auto--build-major-mode-remap-alist
             :around #'my/treesit-auto--cache-remap-a)
 ;; Invalidate cache after installing grammars
 (defun my/treesit-auto-reset-cache ()
   "Reset treesit-auto remap cache (run after installing new grammars)."
   (interactive)
   (setq my/treesit-auto--remap-cache nil)))

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
 :ensure nil
 :commands (eglot-ensure
            eglot-rename
            eglot-format-buffer)
 :hook (python-base-mode . my/eglot-ensure-local)
 :config
 (defun my/eglot-ensure-local ()
  "Run eglot-ensure only on local files."
  (unless (file-remote-p default-directory)
   (eglot-ensure)))
 (add-to-list 'eglot-server-programs
  '(python-base-mode . ("ruff" "server")))
 ;; Disable flymake for Python - using Flycheck instead
 (add-hook 'python-base-mode-hook (lambda () (flymake-mode -1))))

;; Enable intelligent auto-pairing of parens, brackets, and quotes for Python
(add-hook 'python-base-mode-hook #'electric-pair-local-mode)

;; Setup function for Jupyter REPL (defined before use-package)
(defun my/jupyter-repl-setup ()
  "Setup Jupyter REPL with electric-pair and history navigation."
  (electric-pair-local-mode 1)
  ;; Bind arrow keys for history navigation
  (define-key (current-local-map) (kbd "<up>") #'jupyter-repl-history-previous)
  (define-key (current-local-map) (kbd "<down>") #'jupyter-repl-history-next)
  ;; Evil mode integration
  (when (bound-and-true-p evil-mode)
    (evil-local-set-key 'insert (kbd "<up>") #'jupyter-repl-history-previous)
    (evil-local-set-key 'insert (kbd "<down>") #'jupyter-repl-history-next)
    (evil-normalize-keymaps)))

;; Jupyter integration
(use-package jupyter
  :ensure t
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-run-server-repl
             jupyter-eval-defun
             jupyter-eval-region
             jupyter-eval-buffer
             jupyter-eval-line-or-region
             jupyter-repl-pop-to-buffer
             jupyter-repl-interrupt-kernel
             jupyter-repl-associate-buffer)
  :hook (jupyter-repl-mode . my/jupyter-repl-setup)
  :config
  ;; Display Jupyter REPL at bottom
  (add-to-list 'display-buffer-alist
    '("\\*jupyter-repl.*\\*"
      (display-buffer-pop-up-window display-buffer-at-bottom)
      (window-height . 0.3)
      (side . bottom))))

;; Start Jupyter REPL for Python and associate current buffer
(defun my/python-jupyter-repl ()
  "Start Jupyter REPL for Python and associate current buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (call-interactively 'jupyter-run-repl)
    ;; Associate the Python buffer with the REPL
    (with-current-buffer buf
      (when (and (derived-mode-p 'python-mode 'python-ts-mode)
                 (not jupyter-current-client))
        (call-interactively 'jupyter-repl-associate-buffer)))))

;; Ensure jupyter-eval commands work by checking for associated REPL
(defun my/ensure-jupyter-repl ()
  "Ensure a Jupyter REPL is running and associated with current buffer."
  (interactive)
  (unless jupyter-current-client
    (my/python-jupyter-repl)))

;; Wrapper functions that ensure REPL is running
(defun my/jupyter-eval-buffer ()
  "Evaluate buffer in Jupyter, starting REPL if needed."
  (interactive)
  (my/ensure-jupyter-repl)
  (call-interactively 'jupyter-eval-buffer))

(defun my/jupyter-eval-region (start end)
  "Evaluate region in Jupyter, starting REPL if needed."
  (interactive "r")
  (my/ensure-jupyter-repl)
  (jupyter-eval-region start end))

(defun my/jupyter-eval-defun ()
  "Evaluate defun in Jupyter, starting REPL if needed."
  (interactive)
  (my/ensure-jupyter-repl)
  (call-interactively 'jupyter-eval-defun))

(defun my/jupyter-eval-line-or-region ()
  "Evaluate line or region in Jupyter, starting REPL if needed."
  (interactive)
  (my/ensure-jupyter-repl)
  (call-interactively 'jupyter-eval-line-or-region))

;; Bind keys in Python modes
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") #'my/python-jupyter-repl)
  (define-key python-ts-mode-map (kbd "C-c C-p") #'my/python-jupyter-repl)
  ;; Standard send commands mapped to Jupyter
  (define-key python-mode-map (kbd "C-c C-c") #'my/jupyter-eval-defun)
  (define-key python-ts-mode-map (kbd "C-c C-c") #'my/jupyter-eval-defun)
  (define-key python-mode-map (kbd "C-c C-r") #'my/jupyter-eval-region)
  (define-key python-ts-mode-map (kbd "C-c C-r") #'my/jupyter-eval-region))


;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
 :ensure nil
 :commands outline-minor-mode
 :hook
 ((emacs-lisp-mode . outline-minor-mode)
  ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
  ;; folds more visually distinctive and readable.
  (outline-minor-mode
   .
   (lambda()
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
     (set-display-table-slot display-table 'selective-display value)
     (setq buffer-display-table display-table))))))
;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
(use-package outline-indent
 :ensure t
 :commands outline-indent-minor-mode

 :custom
 (outline-indent-ellipsis " ▼")

 :init
 ;; The minor mode can also be automatically activated for a certain modes.
 (add-hook 'python-mode-hook #'outline-indent-minor-mode)
 (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

 (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
 (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

(provide 'usr-programming)
;;; usr-programming.el ends here
