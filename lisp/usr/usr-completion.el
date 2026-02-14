;;; usr-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern completion system:
;; - Corfu (in-buffer completion UI)
;; - Cape (completion backends)
;; - Vertico (vertical minibuffer completion)
;; - Orderless (flexible pattern matching)
;; - Marginalia (rich annotations)
;; - Embark (context actions)
;; - Consult (enhanced commands)
;; - Eshell-carapace (shell completions)

;;; Code:

;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
 :ensure t
 :commands (corfu-mode global-corfu-mode)
 :hook ((prog-mode . corfu-mode)
        (shell-mode . corfu-mode)
        (eshell-mode . corfu-mode))
 :custom
 ;; Hide commands in M-x which do not apply to the current mode.
 (read-extended-command-predicate #'command-completion-default-include-p)
 ;; Disable Ispell completion function. As an alternative try `cape-dict'.
 (text-mode-ispell-word-completion nil)
 (tab-always-indent 'complete)
 ;; Enable Corfu
 :config
 (global-corfu-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
 :ensure t
 :commands (cape-dabbrev cape-file cape-elisp-block cape-elisp-symbol)
 :bind ("C-c p" . cape-prefix-map)
 :init
 ;; Add to the global default value of `completion-at-point-functions' which is
 ;; used by `completion-at-point'.
 (add-hook 'completion-at-point-functions #'cape-dabbrev)
 (add-hook 'completion-at-point-functions #'cape-file)
 (add-hook 'completion-at-point-functions #'cape-elisp-block)
 ;; Add Emacs Lisp symbol completion for elisp-related modes
 (dolist (mode-hook '(emacs-lisp-mode-hook ielm-mode-hook eshell-mode-hook))
   (add-hook mode-hook
             (lambda ()
               (add-hook 'completion-at-point-functions #'cape-elisp-symbol 0 t)))))

;; Carapace provides universal shell completion for eshell, integrating with
;; hundreds of CLI tools (git, docker, kubectl, etc.). It works seamlessly
;; with Corfu (UI), Cape (backends), and Orderless (matching), providing
;; intelligent, context-aware completions with descriptions. Falls back
;; gracefully to Cape backends when unavailable or for unsupported commands.
;; (use-package eshell-carapace
;;  :load-path "lisp"
;;  :custom
;;  (eshell-carapace-executable "carapace")
;;  (eshell-carapace-timeout 2)
;;  :init
;;  (add-hook 'eshell-mode-hook #'eshell-carapace-mode))

;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
 ;; (Note: It is recommended to also enable the savehist package.)
 :ensure t
 :config
 (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
 :ensure t
 :custom
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
 :ensure t
 :commands (marginalia-mode marginalia-cycle)
 :hook (after-init . marginalia-mode))

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
 ;; Embark is an Emacs package that acts like a context menu, allowing
 ;; users to perform context-sensitive actions on selected items
 ;; directly from the completion interface.
 :ensure t
 :commands (embark-act
            embark-dwim
            embark-export
            embark-collect
            embark-bindings
            embark-prefix-help-command)
 :bind
 (("C-." . embark-act)         ;; pick some comfortable binding
  ("C-;" . embark-dwim)        ;; good alternative: M-.
  ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
 :init
 (setq prefix-help-command #'embark-prefix-help-command)
 :config
 ;; Hide the mode line of the Embark live/completions buffers
 (add-to-list 'display-buffer-alist
  '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    nil
    (window-parameters (mode-line-format . none)))))

(use-package embark-consult
 :ensure t
 :hook
 (embark-collect-mode . consult-preview-at-point-mode))

;; Consult offers a suite of commands for efficient searching, previewing, and
;; interacting with buffers, file contents, and more, improving various tasks.
(use-package consult
 :ensure t
 :bind (;; Replace isearch with consult-line
        ("C-s" . consult-line)
        ("C-r" . consult-line)
        ;; C-c bindings in `mode-specific-map'
        ("C-c M-x" . consult-mode-command)
        ("C-c h" . consult-history)
        ("C-c k" . consult-kmacro)
        ("C-c m" . consult-man)
        ("C-c i" . consult-info)
        ([remap Info-search] . consult-info)
        ;; C-x bindings in `ctl-x-map'
        ("C-x M-:" . consult-complex-command)
        ("C-x b" . consult-buffer)
        ("C-x 4 b" . consult-buffer-other-window)
        ("C-x 5 b" . consult-buffer-other-frame)
        ("C-x t b" . consult-buffer-other-tab)
        ("C-x r b" . consult-bookmark)
        ("C-x p b" . consult-project-buffer)
        ;; Custom M-# bindings for fast register access
        ("M-#" . consult-register-load)
        ("M-'" . consult-register-store)
        ("C-M-#" . consult-register)
        ;; Other custom bindings
        ("M-y" . consult-yank-pop)
        ;; M-g bindings in `goto-map'
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flymake)
        ("M-g g" . consult-goto-line)
        ("M-g M-g" . consult-goto-line)
        ("M-g o" . consult-outline)
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)
        ;; M-s bindings in `search-map'
        ("M-s d" . consult-find)
        ("M-s c" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)
        ;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)
        ("M-s e" . consult-isearch-history)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)
        ("M-r" . consult-history))
 ;; Enable automatic preview at point in the *Completions* buffer.
 :hook (completion-list-mode . consult-preview-at-point-mode)
 :init
 ;; Optionally configure the register formatting. This improves the register
 (setq register-preview-delay 0.5
  register-preview-function #'consult-register-format)
 ;; Optionally tweak the register preview window.
 (advice-add #'register-preview :override #'consult-register-window)
 ;; Use Consult to select xref locations with preview
 (setq xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)
 ;; Aggressive asynchronous that yield instantaneous results. (suitable for
 ;; high-performance systems.)
 ;; (setq consult-async-input-debounce 0.02
 ;;       consult-async-input-throttle 0.05
 ;;       consult-async-refresh-delay 0.02)
 :config
 (consult-customize
  consult-theme :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult-source-bookmark consult-source-file-register
  consult-source-recent-file consult-source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.6 any))
 (setq consult-narrow-key "<")
 ;; Suppress git-gutter updates during consult preview
 (add-to-list 'consult-preview-variables '(git-gutter:update-interval . 999)))

(provide 'usr-completion)
;;; usr-completion.el ends here
