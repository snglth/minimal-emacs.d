;;; usr-git.el --- Git integration configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Version control tools:
;; - Magit (Git porcelain)
;; - git-gutter (change indicators)
;; - Forge (GitHub/GitLab integration)
;; - pr-review (PR review interface)
;; - Helper functions for commit messages and PR reviews

;;; Code:

;; Magit is a complete text-based user interface to Git.
(use-package magit
 :ensure t
 :commands (magit-status
            magit-diff
            magit-log
            magit-blame
            magit-log-buffer-file
            magit-clone
            magit-stash
            magit-branch))

;; Git indicators in buffers
(use-package git-gutter
  :hook (prog-mode . my/git-gutter-mode-deferred)
  :config
  (setq git-gutter:update-interval 0.5)
  (defun my/git-gutter-mode-deferred ()
    "Enable git-gutter-mode after a short idle delay."
    (run-with-idle-timer 0.5 nil
     (lambda (buf)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (and (not git-gutter-mode)
                      (derived-mode-p 'prog-mode))
             (git-gutter-mode 1)))))
     (current-buffer))))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(defun my/gptel-write-commit-message ()
 "Generate commit message from staged diff using gptel."
 (interactive)
 (let ((diff (shell-command-to-string "git diff --cached"))
       (buf (current-buffer)))
  (if (string-empty-p diff)
   (message "No staged changes to generate commit message for")
   (message "Generating commit message...")
   (gptel-request
    (format "Write a concise git commit message for this diff. Follow conventional commit format (type(scope): description). Output only the message, no explanation:\n\n%s" diff)
    :callback (lambda (response info)
               (if response
                (with-current-buffer buf
                 (goto-char (point-min))
                 (insert response)
                 (message "Commit message generated"))
                (message "Failed to generate: %s" (plist-get info :status))))))))

(with-eval-after-load 'git-commit
 (define-key git-commit-mode-map (kbd "M-c") #'my/gptel-write-commit-message))

;; GitHub PR review porcelain
(use-package pr-review
 :vc (:url "https://github.com/blahgeek/emacs-pr-review" :rev :newest)
 :config
 (add-to-list 'browse-url-default-handlers
  '(pr-review-url-parse . pr-review-open-url))
 ;; Define evil commands after evil loads
 (with-eval-after-load 'evil
  (evil-ex-define-cmd "prr" #'pr-review)
  (evil-ex-define-cmd "prs" #'pr-review-search)
  (evil-ex-define-cmd "prn" #'pr-review-notification)))

;; Install websocket from GitHub to avoid ELPA signature issues
(use-package websocket
 :vc (:url "https://github.com/ahyatt/emacs-websocket" :rev :newest)
 :defer t)

(use-package forge
 :ensure t
 :after magit)

(defun my/claude-code-review-pr ()
 "Send current Forge PR to Claude Code for review."
 (interactive)
 (require 'claude-code-ide)
 (unless (derived-mode-p 'forge-pullreq-mode)
  (user-error "Not in a Forge pull request buffer"))
 (let ((pr-number (oref forge-buffer-topic number)))
  (claude-code-ide-send-prompt
   (format "/pr-review %s" pr-number))))

(with-eval-after-load 'forge
 (define-key forge-pullreq-mode-map (kbd "M-c") #'my/claude-code-review-pr))

(provide 'usr-git)
;;; usr-git.el ends here
