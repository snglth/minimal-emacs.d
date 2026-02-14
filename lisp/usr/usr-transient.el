;;; usr-transient.el --- Transient menu system -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive transient menu system for navigation and actions.
;; Provides a vim-like leader key interface via SPC in normal/visual modes.

;;; Code:

;; Transient menus
(use-package transient
 :vc (:url "https://github.com/magit/transient" :rev :newest)
 :demand t
 :bind ("C-c SPC" . transient-leader)
 :config
 ;; Helper for project terminal
 (defun my/terminal-new ()
  "Open new terminal in current directory.
Uses MisTTY for both local and remote hosts.
Always creates a new terminal buffer."
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-in-direction)
           (direction . bottom)
           (window-height . 0.5)
           (inhibit-same-window . t))))
    (mistty-create nil t)))

 (defun my/terminal-mistty ()
  "Open new MisTTY terminal in current directory.
Always creates a new terminal buffer."
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-in-direction)
           (direction . bottom)
           (window-height . 0.5)
           (inhibit-same-window . t))))
    (mistty-create nil t)))

 (defun my/terminal-eshell ()
  "Open new eshell in current directory.
Always creates a new eshell buffer."
  (interactive)
  (eshell 'N))

 (defun my/terminal-eat ()
  "Open new eat terminal in current directory.
Always creates a new terminal buffer."
  (interactive)
  (eat))

 (defun my/terminal-project ()
  "Open new terminal in project root.
Always creates a new terminal buffer."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
   (my/terminal-new)))

 (defun my/terminal-project-mistty ()
  "Open new MisTTY in project root.
Always creates a new terminal buffer."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (display-buffer-overriding-action
         '((display-buffer-in-direction)
           (direction . bottom)
           (window-height . 0.3)
           (inhibit-same-window . t))))
    (mistty-create nil t)))

 (defun my/terminal-project-eshell ()
  "Open new eshell in project root.
Always creates a new eshell buffer."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
   (my/terminal-eshell)))

 (defun my/terminal-kill ()
  "Kill current terminal buffer."
  (interactive)
  (when (or (derived-mode-p 'eat-mode)
        (derived-mode-p 'eshell-mode)
        (derived-mode-p 'mistty-mode))
   (kill-buffer (current-buffer))))

 (defun my/switch-to-messages ()
  "Switch to *Messages* buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

 ;; Master leader menu
 (transient-define-prefix transient-leader ()
  "Leader menu"
  [["Navigation"
    ("s" "search" transient-search)
    ("f" "files" transient-files-menu)
    ("b" "buffers" transient-buffers)
    ("p" "project" transient-project)]
   ["Actions"
    ("w" "windows" transient-windows)
    ("g" "git" transient-git-menu)
    ("c" "completion" transient-cape)
    ("e" "embark" transient-embark)]
   ["Tools"
    ("u" "undo" transient-undo)
    ("x" "errors" transient-flycheck)
    ("z" "fold" transient-fold)
    ("t" "terminal" transient-terminal)
    ("E" "eval" transient-eval)]
   ["AI"
    ("a" "gptel" gptel-menu)
    ("A" "claude" claude-code-ide-menu)]
   ["Other"
    ("h" "help" transient-help)
    ("q" "quit" transient-quit)
    ("SPC" "M-x" execute-extended-command)]])

 ;; Search menu
 (transient-define-prefix transient-search ()
  "Search"
  [["Buffer"
    ("s" "line" consult-line)
    ("o" "outline" consult-outline)
    ("i" "imenu" consult-imenu)
    ("l" "line-multi" consult-line-multi)
    ("k" "keep-lines" consult-keep-lines)
    ("u" "focus-lines" consult-focus-lines)]
   ["Navigate"
    ("g" "goto-line" consult-goto-line)
    ("m" "mark" consult-mark)
    ("M" "global-mark" consult-global-mark)]
   ["Project"
    ("r" "ripgrep" consult-ripgrep)
    ("G" "git-grep" consult-git-grep)
    ("f" "find" consult-find)
    ("F" "locate" consult-locate)
    ("I" "imenu-multi" consult-imenu-multi)]])

 ;; Files menu
 (transient-define-prefix transient-files-menu ()
  "Files"
  [["Open"
    ("f" "find file" find-file)
    ("r" "recent" consult-recent-file)
    ("b" "bookmark" consult-bookmark)
    ("d" "dired" dired)]
   ["Save"
    ("s" "save" save-buffer)
    ("S" "save all" save-some-buffers)
    ("D" "diff" diff-buffer-with-file)]])

 ;; Buffers menu
 (transient-define-prefix transient-buffers ()
  "Buffers"
  [["Switch"
    ("b" "switch in this window" consult-buffer)
    ("B" "switch in other window" consult-buffer-other-window)
    ("n" "next" next-buffer)
    ("p" "prev" previous-buffer)]
   ["Manage"
    ("k" "kill" kill-current-buffer)
    ("K" "kill & window" kill-buffer-and-window)
    ("r" "revert" revert-buffer)]
   ["Special"
    ("s" "scratch" scratch-buffer)
    ("m" "messages" my/switch-to-messages)
    ("y" "yank-pop" consult-yank-pop)]
   ["Registers"
    ("R" "register" consult-register)
    ("#" "load register" consult-register-load)
    ("'" "store register" consult-register-store)]])

 ;; Project menu
 (transient-define-prefix transient-project ()
  "Project"
  [["Navigate"
    ("p" "switch" project-switch-project)
    ("f" "find file" project-find-file)
    ("b" "buffers" consult-project-buffer)
    ("d" "dired" project-dired)]
   ["Actions"
    ("g" "grep" consult-ripgrep)
    ("c" "compile" project-compile)]])

 ;; Windows menu
 (transient-define-prefix transient-windows ()
  "Windows"
  [["Focus"
    ("h" "left" evil-window-left)
    ("j" "down" evil-window-down)
    ("k" "up" evil-window-up)
    ("l" "right" evil-window-right)]
   ["Move"
    ("H" "← move" evil-window-move-far-left)
    ("J" "↓ move" evil-window-move-very-bottom)
    ("K" "↑ move" evil-window-move-very-top)
    ("L" "→ move" evil-window-move-far-right)]
   ["Split"
    ("s" "horizontal" evil-window-split)
    ("v" "vertical" evil-window-vsplit)]
   ["Manage"
    ("d" "delete" evil-window-delete)
    ("o" "only" delete-other-windows)
    ("=" "balance" balance-windows)
    ("m" "maximize" maximize-window)
    ("r" "resize" transient-window-resize)
    ("t" "transpose" transpose-frame)
    ("R" "rotate frame" rotate-frame-clockwise)]
   ["Layout"
    ("n" "next layout" rotate-layout)
    ("w" "rotate windows" rotate-window)]])

 ;; Window resize menu (stays open for repeated adjustments)
 (transient-define-prefix transient-window-resize ()
  "Resize windows"
  :transient-suffix 'transient--do-stay
  [["Resize"
    ("h" "← narrower" shrink-window-horizontally)
    ("l" "→ wider" enlarge-window-horizontally)
    ("k" "↑ shorter" shrink-window)
    ("j" "↓ taller" enlarge-window)]
   ["Other"
    ("=" "balance" balance-windows :transient nil)
    ("m" "maximize" maximize-window :transient nil)
    ("q" "quit" transient-quit-all)]])

 ;; Git menu
 (transient-define-prefix transient-git-menu ()
  "Git"
  [["Status"
    ("g" "status" magit-status)
    ("d" "diff" magit-diff)
    ("l" "log" magit-log)]
   ["File"
    ("b" "blame" magit-blame)
    ("f" "file log" magit-log-buffer-file)
    ("p" "git-grep" consult-git-grep)]
   ["Actions"
    ("c" "clone" magit-clone)
    ("S" "stash" magit-stash)
    ("B" "branch" magit-branch)]])

 ;; Help menu
 (transient-define-prefix transient-help ()
  "Help"
  [["Describe"
    ("f" "function" describe-function)
    ("v" "variable" describe-variable)
    ("k" "key" describe-key)
    ("m" "mode" describe-mode)
    ("p" "package" describe-package)]
   ["Documentation"
    ("i" "info" consult-info)
    ("M" "man" consult-man)
    ("a" "apropos" apropos)]
   ["Other"
    ("b" "bindings" embark-bindings)
    ("K" "kmacro" consult-kmacro)]])

 ;; Quit menu
 (transient-define-prefix transient-quit ()
  "Quit"
  [["Quit"
    ("q" "quit emacs" save-buffers-kill-terminal)
    ("r" "restart" restart-emacs)
    ("f" "delete frame" delete-frame)]])

 ;; Embark menu
 (transient-define-prefix transient-embark ()
  "Embark"
  [["Actions"
    ("a" "act" embark-act)
    ("d" "dwim" embark-dwim)
    ("e" "export" embark-export)
    ("c" "collect" embark-collect)
    ("b" "bindings" embark-bindings)]])

 ;; Cape completion menu
 (transient-define-prefix transient-cape ()
  "Completion"
  [["Text"
    ("d" "dabbrev" cape-dabbrev)
    ("l" "line" cape-line)
    ("w" "dict" cape-dict)
    ("a" "abbrev" cape-abbrev)]
   ["Code"
    ("f" "file" cape-file)
    ("s" "symbol" cape-elisp-symbol)
    ("e" "elisp" cape-elisp-block)
    ("k" "keyword" cape-keyword)]])

 ;; Undo menu (transient for repeatable actions)
 (transient-define-prefix transient-undo ()
  "Undo/Redo"
  :transient-suffix 'transient--do-stay
  [["Undo"
    ("u" "undo" undo-fu-only-undo)
    ("r" "redo" undo-fu-only-redo)
    ("R" "redo all" undo-fu-only-redo-all :transient nil)]])

 ;; Flycheck menu
 (transient-define-prefix transient-flycheck ()
  "Flycheck"
  :transient-suffix 'transient--do-stay
  [["Navigate"
    ("n" "next" flycheck-next-error)
    ("p" "prev" flycheck-previous-error)
    ("l" "list" flycheck-list-errors :transient nil)
    ("e" "compile-error" consult-compile-error :transient nil)
    ("f" "flymake" consult-flymake :transient nil)]
   ["Actions"
    ("c" "check" flycheck-buffer :transient nil)
    ("C" "clear" flycheck-clear :transient nil)
    ("s" "select checker" flycheck-select-checker :transient nil)]
   ["Info"
    ("v" "verify" flycheck-verify-setup :transient nil)
    ("d" "describe" flycheck-describe-checker :transient nil)
    ("x" "disable" flycheck-disable-checker :transient nil)]])

 ;; Fold menu
 (transient-define-prefix transient-fold ()
  "Fold"
  :transient-suffix 'transient--do-stay
  [["Show/Hide"
    ("a" "show all" outline-show-all :transient nil)
    ("A" "hide all" outline-hide-body :transient nil)
    ("o" "show entry" outline-show-entry)
    ("c" "hide entry" outline-hide-entry)]
   ["Subtree"
    ("s" "show subtree" outline-show-subtree)
    ("h" "hide subtree" outline-hide-subtree)
    ("l" "show branches" outline-show-branches)
    ("d" "hide leaves" outline-hide-leaves)
    ("t" "toggle" outline-toggle-children)]
   ["Navigate"
    ("n" "next" outline-next-visible-heading)
    ("p" "prev" outline-previous-visible-heading)
    ("f" "forward" outline-forward-same-level)
    ("b" "backward" outline-backward-same-level)]])

 ;; Eval menu - mode-aware dispatcher
 (transient-define-prefix transient-eval ()
  "Evaluate code (mode-aware)"
  (interactive)
  (cond
   ((derived-mode-p 'python-mode 'python-ts-mode)
    (transient-eval-python))
   ((derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
    (transient-eval-elisp))
   (t
    (transient-eval-elisp))))

 ;; Elisp eval menu
 (transient-define-prefix transient-eval-elisp ()
  "Evaluate Elisp"
  [["Eval"
    ("d" "defun" eval-defun)
    ("r" "region" eval-region)
    ("b" "buffer" eval-buffer)
    ("e" "expression" eval-expression)
    ("l" "last sexp" eval-last-sexp)]
   ["Load"
    ("f" "file" load-file)
    ("L" "library" load-library)]])

 ;; Python eval menu
 (transient-define-prefix transient-eval-python ()
  "Evaluate Python"
  [["Send to REPL"
    ("d" "defun" my/jupyter-eval-defun)
    ("r" "region" my/jupyter-eval-region)
    ("b" "buffer" my/jupyter-eval-buffer)
    ("l" "line" my/jupyter-eval-line-or-region)]
   ["REPL"
    ("p" "start REPL" my/python-jupyter-repl)
    ("z" "switch to REPL" jupyter-repl-pop-to-buffer)
    ("i" "interrupt kernel" jupyter-repl-interrupt-kernel)]])

 ;; Terminal menu
 (transient-define-prefix transient-terminal ()
  "Terminal"
  [["Current Directory"
    ("t" "new terminal (mistty)" my/terminal-new)
    ("m" "new mistty" my/terminal-mistty)
    ("e" "new eshell" my/terminal-eshell)
    ("a" "new eat" my/terminal-eat)]
   ["Project Root"
    ("p" "new terminal (mistty)" my/terminal-project)
    ("M" "new mistty" my/terminal-project-mistty)
    ("E" "new eshell" my/terminal-project-eshell)]
   ["Actions"
    ("k" "kill" my/terminal-kill)]]))

(use-package casual
 :ensure t
 :config
 (casual-ediff-install) ; run this to enable Casual Ediff
 (add-hook 'ediff-keymap-setup-hook
  (lambda ()
   (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu)))

 (defun my/ediff-copy-all-B-to-A ()
  "Copy all difference regions from buffer B to buffer A."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (dotimes (i ediff-number-of-differences)
   (ediff-copy-diff i nil 'A nil
    (ediff-get-region-contents i 'B ediff-control-buffer)))
  (message "Copied all %d regions from B to A" ediff-number-of-differences))

 (transient-append-suffix 'casual-ediff-tmenu '(0 2 -1)
  '("Ba" "Accept all B→A" my/ediff-copy-all-B-to-A
    :transient t
    :if (lambda () (not (casual-ediff--buffer-read-only-p ediff-buffer-A)))))

 (transient-append-suffix 'casual-ediff-tmenu '(0 2 -1)
  '("Bq" "Accept all B→A & quit" gptel-ediff--accept-all-and-quit
    :transient nil
    :if (lambda () (not (casual-ediff--buffer-read-only-p ediff-buffer-A)))))

 (transient-append-suffix 'casual-ediff-tmenu '(0 2 -1)
  '("Q" "Quit with comment" gptel-ediff--quit-with-comment
    :transient nil)))

(provide 'usr-transient)
;;; usr-transient.el ends here
