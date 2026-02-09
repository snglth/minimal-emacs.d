;;; usr-ai.el --- AI tools configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; AI/LLM integration:
;; - GPTel (LLM interface)
;; - gptel-agent (agentic tools)
;; - claude-code-ide (Claude Code integration)
;; - agent-shell (ACP tools)
;; - prism (rainbow parentheses)

;;; Code:

;; LLM stuff
;; General AI interface for Emacs
(use-package gptel
 :vc (:url "https://github.com/karthink/gptel" :rev :newest)
 :config
 (require 'gptel-integrations) ;; <- for mcp
 (setq
  gptel-default-mode #'org-mode
  gptel-model "anthropic/claude-opus-4.6"
  gptel--system-message
  (concat "You are an experienced Site Reliability Engineer "
          "administrating a fleet of Linux machines and Ceph clusters "
          "across multiple regions. Skilled in operations automation, "
          "infrastructure as code, monitoring, incident response, "
          "and capacity planning.\n\n"
          "Be terse and casual. Treat me as an expert. "
          "Give the answer immediately, explain after if needed. "
          "Anticipate my needs and suggest solutions I didn't think about. "
          "Value good arguments over authorities. "
          "Consider new technologies and contrarian ideas. "
          "Flag speculation. No moral lectures. "
          "Prefer functional and declarative styles. "
          "It's utmost imprortant to maintain concise data model and clearly define data structures in code "
          "Provide actionable commands and scripts when appropriate.")
  gptel-backend (gptel-make-openai "OpenRouter"               ;Any name you want
                 :host "openrouter.ai"
                 :endpoint "/api/v1/chat/completions"
                 :key 'gptel-api-key
                 :stream t
                 :models '(anthropic/claude-opus-4.6
                           openai/gpt-oss-safeguard-20b:nitro
                           x-ai/grok-4.1-fast)))

 ;; Agentic harness for gptel
 (use-package gptel-agent
  :vc ( :url "https://github.com/karthink/gptel-agent"
        :rev :newest)
  :config
  (setq gptel-agent-dirs
   (list (expand-file-name "agents" user-emacs-directory)))
  (gptel-agent-update)
  ;; Auto-enable gptel-agent tools in gptel buffers
  (add-hook 'gptel-mode-hook
   (lambda ()
    (when (and (boundp 'gptel--known-tools)
           (null gptel-tools))
     (setq-local gptel-tools
      (mapcar #'cdr (cdr (assoc "gptel-agent" gptel--known-tools))))))))

 ;; Dynamic buffer tail context
 (defun gptel-context--update-buffer-tail (&rest _)
  "Update tail context overlays in current buffer after changes."
  (let ((found nil))
   (dolist (ov (overlays-in (point-min) (point-max)))
    (when-let ((n (overlay-get ov 'gptel-context-tail-lines)))
     (setq found t)
     (let ((start (save-excursion
                   (goto-char (point-max))
                   (forward-line (- n))
                   (point)))
           (end (point-max)))
      (move-overlay ov start end))))
   ;; Clean up hook if no tail contexts remain (deleted via gptel-context UI)
   (unless found
    (remove-hook 'after-change-functions #'gptel-context--update-buffer-tail t))))

 (defun gptel-context-add-buffer-tail (buffer n)
  "Add the last N lines of BUFFER to gptel context.
The context is dynamic and updates as new lines appear."
  (interactive
   (list (read-buffer "Buffer: " (current-buffer) t)
    (read-number "Number of lines: " 100)))
  (let ((buf (get-buffer buffer)))
   (with-current-buffer buf
    ;; Remove existing tail contexts for this buffer
    (dolist (ov (overlays-in (point-min) (point-max)))
     (when (overlay-get ov 'gptel-context-tail-lines)
      (delete-overlay ov)))
    (remove-hook 'after-change-functions #'gptel-context--update-buffer-tail t)
    ;; Add new context
    (let* ((start (save-excursion
                   (goto-char (point-max))
                   (forward-line (- n))
                   (point)))
           (end (point-max))
           (ov (gptel-context--add-region buf start end)))
     ;; Mark as dynamic tail context
     (overlay-put ov 'gptel-context-tail-lines n)
     ;; Set up update hook
     (add-hook 'after-change-functions #'gptel-context--update-buffer-tail nil t)))
   (message "Added dynamic tail context (%d lines) for %s" n (buffer-name buf))))

 (with-eval-after-load 'gptel-transient
  (transient-define-suffix gptel--suffix-context-add-buffer-tail ()
   "Add last N lines of a buffer to gptel's context."
   :transient 'transient--do-stay
   :key "-t"
   :description "Add buffer tail to context"
   (interactive)
   (call-interactively #'gptel-context-add-buffer-tail))

  (transient-append-suffix 'gptel-menu "-b"
   '("-t" "Add buffer tail to context" gptel--suffix-context-add-buffer-tail)))

;; install claude-code-ide.el
(use-package claude-code-ide
 :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
 :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
 :custom
 (claude-code-ide-terminal-backend 'eat)
 :config
 (claude-code-ide-emacs-tools-setup))

;; ACP stuff
(use-package agent-shell
 :ensure t)

;; Funny colors
(use-package prism
 :vc (:url "https://github.com/alphapapa/prism.el" :rev :newest)
 :config
 (prism-set-colors :num 16
  :desaturations (cl-loop for i from 0 below 16
                  collect (* i 2.5))
  :lightens (cl-loop for i from 0 below 16
             collect (* i 2.5))
  :colors (list "dodgerblue" "medium sea green" "sandy brown")

  :comments-fn
  (lambda (color)
   (prism-blend color
    (face-attribute 'font-lock-comment-face :foreground) 0.25))

  :strings-fn
  (lambda (color)
   (prism-blend color "white" 0.5)))))

(provide 'usr-ai)
;;; usr-ai.el ends here
