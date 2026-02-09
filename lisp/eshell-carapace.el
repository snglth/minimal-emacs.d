;;; eshell-carapace.el --- Carapace completion for Eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: eshell, completion, carapace
;; URL: https://github.com/rsteube/carapace-bin

;;; Commentary:

;; This package integrates carapace (https://github.com/rsteube/carapace-bin)
;; with Emacs eshell to provide intelligent, context-aware completions for
;; hundreds of CLI tools (git, docker, kubectl, etc.).
;;
;; Usage:
;;   (add-hook 'eshell-mode-hook #'eshell-carapace-setup)
;;
;; The integration works seamlessly with Corfu, Cape, and Orderless.
;; If carapace is unavailable or fails, completion gracefully falls back
;; to other backends (cape-file, cape-dabbrev, etc.).

;;; Code:

(require 'eshell)
(require 'json)

;;; Customization

(defgroup eshell-carapace nil
  "Carapace completion integration for Eshell."
  :group 'eshell-module
  :prefix "eshell-carapace-")

(defcustom eshell-carapace-executable "carapace"
  "Path to the carapace executable.
Can be a simple command name (\"carapace\") if it's in PATH,
or an absolute path (\"/opt/homebrew/bin/carapace\")."
  :type 'string
  :group 'eshell-carapace)

(defcustom eshell-carapace-timeout 2
  "Timeout in seconds for carapace invocation.
If carapace takes longer than this, abort and return nil."
  :type 'number
  :group 'eshell-carapace)

(defcustom eshell-carapace-debug nil
  "Enable debug messages for carapace integration.
When non-nil, logs carapace invocations and responses to *Messages*."
  :type 'boolean
  :group 'eshell-carapace)

;;; Executable Detection

(defvar eshell-carapace--executable-cache nil
  "Cached result of executable check.
Format: (EXECUTABLE-PATH . TIMESTAMP)")

(defvar eshell-carapace--cache-ttl 300
  "Cache TTL in seconds (5 minutes).")

(defun eshell-carapace--cache-expired-p ()
  "Return non-nil if executable cache is expired or empty."
  (or (null eshell-carapace--executable-cache)
      (> (float-time (time-since (cdr eshell-carapace--executable-cache)))
         eshell-carapace--cache-ttl)))

(defun eshell-carapace--find-executable ()
  "Find carapace executable and return full path, or nil if not found."
  (when-let ((exe (executable-find eshell-carapace-executable)))
    (when (file-executable-p exe)
      exe)))

(defun eshell-carapace--ensure-executable ()
  "Ensure carapace executable is available.
Returns executable path if available, nil otherwise.
Uses cache to avoid repeated filesystem checks."
  (when (eshell-carapace--cache-expired-p)
    (let ((exe (eshell-carapace--find-executable)))
      (setq eshell-carapace--executable-cache
            (when exe (cons exe (current-time))))))
  (car-safe eshell-carapace--executable-cache))

;;; Command Line Parsing

(defun eshell-carapace--parse-current-command ()
  "Parse eshell command at point.
Returns plist with keys:
  :command      - The command name (string)
  :input-line   - Full input line text (for CARAPACE_COMPLINE)
  :prefix-start - Buffer position of completion start
  :prefix-end   - Buffer position of completion end
Returns nil if not on a valid command line."
  (save-excursion
    (let* ((current-pos (point))
           ;; Use eshell-bol to get to beginning of line after prompt
           (input-start (save-excursion
                         (eshell-bol)
                         (point))))
      (when eshell-carapace-debug
        (message "eshell-carapace: parse input-start=%s current-pos=%s" input-start current-pos))
      (when (and input-start (>= current-pos input-start))
        (let* ((input-line (buffer-substring-no-properties input-start current-pos))
               ;; Split on whitespace (simple parsing for MVP)
               (words (split-string input-line nil t))
               (command (car words))
               ;; Find current word boundaries
               (current-word-start (save-excursion
                                    (skip-chars-backward "^ \t\n")
                                    (point))))

          (when eshell-carapace-debug
            (message "eshell-carapace: input-line='%s' command='%s'" input-line command))

          (when command
            (list :command command
                  :input-line input-line
                  :prefix-start current-word-start
                  :prefix-end current-pos)))))))

;;; Carapace Invocation

(defvar eshell-carapace--running-process nil
  "Currently running carapace process, if any.")

(defun eshell-carapace--call-carapace (command input-line)
  "Call carapace binary and return parsed JSON data.
COMMAND is the command name (e.g., \"git\").
INPUT-LINE is the full command line text to complete.
Returns alist from JSON response, or nil on error.
Uses timeout from `eshell-carapace-timeout' to prevent hanging."
  (when-let ((exe (eshell-carapace--ensure-executable)))
    ;; Kill any existing running process
    (when (and eshell-carapace--running-process
               (process-live-p eshell-carapace--running-process))
      (when eshell-carapace-debug
        (message "eshell-carapace: killing previous hanging process"))
      (kill-process eshell-carapace--running-process)
      (setq eshell-carapace--running-process nil))

    (condition-case err
        (with-timeout (eshell-carapace-timeout
                      (when eshell-carapace-debug
                        (message "eshell-carapace: timeout after %s seconds for '%s'"
                                eshell-carapace-timeout command))
                      (when (and eshell-carapace--running-process
                                 (process-live-p eshell-carapace--running-process))
                        (kill-process eshell-carapace--running-process)
                        (setq eshell-carapace--running-process nil))
                      nil)
          (with-temp-buffer
            ;; Split input-line into words for xargs-style invocation
            (let* ((words (split-string input-line nil t))
                   ;; Set CARAPACE_COMPLINE environment variable
                   (process-environment (cons (format "CARAPACE_COMPLINE=%s" input-line)
                                             process-environment))
                   ;; Call: carapace <command> export <full-command-line-words>
                   ;; This mimics: echo "git -" | xargs carapace git export
                   ;; which becomes: carapace git export git -
                   exit-code)

              ;; Start the process
              (setq eshell-carapace--running-process
                    (apply #'start-process "carapace" (current-buffer) exe
                           command "export" words))

              ;; Wait for it to complete
              (while (process-live-p eshell-carapace--running-process)
                (accept-process-output eshell-carapace--running-process 0.1))

              (setq exit-code (process-exit-status eshell-carapace--running-process))
              (setq eshell-carapace--running-process nil)

              (when eshell-carapace-debug
                (message "eshell-carapace: CARAPACE_COMPLINE='%s'" input-line)
                (message "eshell-carapace: invoked '%s %s export %s'"
                         exe command (string-join words " "))
                (message "eshell-carapace: exit code %s, output:\n%s"
                         exit-code (buffer-string)))

              (when (and exit-code (not (zerop exit-code)) eshell-carapace-debug)
                (message "eshell-carapace: completer '%s' failed with exit code %d (this may be a carapace bug)"
                         command exit-code))

              (when (and exit-code (zerop exit-code))
                (goto-char (point-min))
                (let ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol))
                  (json-read))))))
      (error
       (when eshell-carapace-debug
         (message "eshell-carapace: error calling carapace: %S" err))
       (when (and eshell-carapace--running-process
                  (process-live-p eshell-carapace--running-process))
         (kill-process eshell-carapace--running-process)
         (setq eshell-carapace--running-process nil))
       nil))))

;;; JSON Parsing and Candidate Creation

(defun eshell-carapace--parse-completions (json-data)
  "Parse carapace JSON response into completion candidates.
JSON-DATA is the alist returned by `json-read'.
Returns list of strings with text properties for metadata."
  (when-let ((values (alist-get 'values json-data)))
    (let ((candidates nil))
      (dolist (item values)
        (when-let ((value (alist-get 'value item)))
          (let* ((description (or (alist-get 'description item) ""))
                 (tag (or (alist-get 'tag item) ""))
                 ;; Create candidate with text properties
                 (candidate (propertize value
                                       'eshell-carapace-value value
                                       'eshell-carapace-description description
                                       'eshell-carapace-tag tag)))
            (push candidate candidates))))
      (nreverse candidates))))

;;; Annotation and Metadata Functions

(defun eshell-carapace--annotate (candidate)
  "Return annotation string for CANDIDATE.
Displays the description from carapace in the completion popup."
  (when-let ((desc (get-text-property 0 'eshell-carapace-description candidate)))
    (unless (string-empty-p desc)
      (concat " " (propertize desc 'face 'completions-annotations)))))

(defun eshell-carapace--kind (candidate)
  "Return completion kind for CANDIDATE.
Maps carapace tags to company-kind symbols for icon display in Corfu."
  (let ((tag (get-text-property 0 'eshell-carapace-tag candidate)))
    (pcase tag
      ("files" 'file)
      ("directories" 'folder)
      ("commands" 'function)
      ("flags" 'keyword)
      ("options" 'keyword)
      (_ 'text))))

;;; Main CAPF Function

(defun eshell-carapace-completion-at-point ()
  "Main completion-at-point-functions hook for carapace integration.
Provides completions for eshell commands using carapace.
Returns nil to allow fallback to other backends if:
  - Not in eshell-mode
  - Carapace executable not available
  - Point not on command line
  - Carapace invocation fails"
  (when (and (derived-mode-p 'eshell-mode)
             (eshell-carapace--ensure-executable)
             (>= (point) (eshell-beginning-of-input)))
    (when-let* ((context (eshell-carapace--parse-current-command))
                (command (plist-get context :command)))
      (let* ((input-line (plist-get context :input-line))
             (json-data (eshell-carapace--call-carapace command input-line))
             (completions (when json-data
                           (eshell-carapace--parse-completions json-data))))

        (when (and eshell-carapace-debug completions)
          (message "eshell-carapace: found %d completions for '%s'"
                   (length completions) command))

        (when completions
          (list (plist-get context :prefix-start)
                (plist-get context :prefix-end)
                completions
                :exclusive 'no  ; Allow cascade to cape-file, cape-dabbrev
                :annotation-function #'eshell-carapace--annotate
                :company-kind #'eshell-carapace--kind))))))

;;; Minor Mode

;;;###autoload
(define-minor-mode eshell-carapace-mode
  "Toggle carapace completion in eshell.
When enabled, adds carapace completion to `completion-at-point-functions'
and removes `pcomplete-completions-at-point' to give carapace priority."
  :lighter " Carapace"
  :group 'eshell-carapace
  (if eshell-carapace-mode
      (progn
        ;; Remove pcomplete to let carapace handle command completion
        (remove-hook 'completion-at-point-functions
                     #'pcomplete-completions-at-point t)
        ;; Add carapace at front with depth -10 so it runs before cape-file/cape-dabbrev
        (add-hook 'completion-at-point-functions
                  #'eshell-carapace-completion-at-point -10 t)
        (when eshell-carapace-debug
          (message "eshell-carapace-mode enabled in buffer %s" (buffer-name))
          (message "eshell-carapace: completion-at-point-functions = %S"
                   completion-at-point-functions)))
    ;; Restore pcomplete when disabling
    (remove-hook 'completion-at-point-functions
                 #'eshell-carapace-completion-at-point t)
    (add-hook 'completion-at-point-functions
              #'pcomplete-completions-at-point nil t)
    (when eshell-carapace-debug
      (message "eshell-carapace-mode disabled in buffer %s" (buffer-name)))))

;;; Setup Function

;;;###autoload
(defun eshell-carapace-setup ()
  "Setup carapace completion for the current eshell buffer.
Intended to be added to `eshell-mode-hook'."
  (eshell-carapace-mode 1))

;;; Cache Management

(defun eshell-carapace-clear-cache ()
  "Clear the carapace executable cache.
Useful if the executable was installed or moved while Emacs is running."
  (interactive)
  (setq eshell-carapace--executable-cache nil)
  (message "eshell-carapace: cache cleared"))

(defun eshell-carapace-kill-process ()
  "Kill any hanging carapace process.
Use this if carapace hangs and you want to abort it."
  (interactive)
  (when (and eshell-carapace--running-process
             (process-live-p eshell-carapace--running-process))
    (kill-process eshell-carapace--running-process)
    (setq eshell-carapace--running-process nil)
    (message "eshell-carapace: killed hanging process"))
  (unless eshell-carapace--running-process
    (message "eshell-carapace: no process running")))

;;; Diagnostics

(defun eshell-carapace-status ()
  "Show quick status of carapace integration in current buffer.
Run from eshell to check if carapace is active."
  (interactive)
  (message "Mode: %s | CAPF registered: %s | Functions: %S"
           (if (bound-and-true-p eshell-carapace-mode) "ON" "OFF")
           (if (memq 'eshell-carapace-completion-at-point completion-at-point-functions) "YES" "NO")
           completion-at-point-functions))

(defun eshell-carapace-diagnose ()
  "Diagnose carapace integration setup and display status.
Run this from an eshell buffer to troubleshoot completion issues."
  (interactive)
  (let ((output (with-temp-buffer
                  (insert "=== Eshell Carapace Diagnostics ===\n\n")

                  ;; Check if in eshell
                  (insert (format "1. Buffer mode: %s\n" major-mode))
                  (if (derived-mode-p 'eshell-mode)
                      (insert "   ✓ In eshell-mode\n")
                    (insert "   ✗ NOT in eshell-mode (run this from an eshell buffer)\n"))

                  ;; Check if mode is enabled
                  (insert (format "\n2. eshell-carapace-mode: %s\n"
                                (if (bound-and-true-p eshell-carapace-mode) "enabled" "disabled")))
                  (if (bound-and-true-p eshell-carapace-mode)
                      (insert "   ✓ Mode is enabled\n")
                    (insert "   ✗ Mode is NOT enabled\n   Fix: Run M-x eshell-carapace-mode\n"))

                  ;; Check if CAPF is in the list
                  (insert (format "\n3. completion-at-point-functions: %s\n"
                                completion-at-point-functions))
                  (if (memq 'eshell-carapace-completion-at-point completion-at-point-functions)
                      (insert "   ✓ carapace CAPF is registered\n")
                    (insert "   ✗ carapace CAPF is NOT registered\n"))

                  ;; Check executable
                  (insert (format "\n4. Carapace executable: %s\n" eshell-carapace-executable))
                  (let ((exe (eshell-carapace--find-executable)))
                    (if exe
                        (insert (format "   ✓ Found at: %s\n" exe))
                      (insert "   ✗ NOT FOUND in PATH\n   Fix: Install carapace or set eshell-carapace-executable\n")))

                  ;; Check cache
                  (insert (format "\n5. Executable cache: %s\n"
                                (if eshell-carapace--executable-cache
                                    (format "%s (age: %.1fs)"
                                          (car eshell-carapace--executable-cache)
                                          (float-time (time-since (cdr eshell-carapace--executable-cache))))
                                  "empty")))

                  ;; Test carapace call
                  (insert "\n6. Test carapace call (git):\n")
                  (condition-case err
                      (let ((result (eshell-carapace--call-carapace "git" "git")))
                        (if result
                            (insert (format "   ✓ Success: Got %d completions\n"
                                          (length (alist-get 'values result))))
                          (insert "   ✗ Call returned nil\n")))
                    (error (insert (format "   ✗ Error: %S\n" err))))

                  ;; Check for running process
                  (insert (format "\n7. Running process: %s\n"
                                (if (and eshell-carapace--running-process
                                         (process-live-p eshell-carapace--running-process))
                                    (format "ACTIVE (PID: %s)" (process-id eshell-carapace--running-process))
                                  "none")))
                  (when (and eshell-carapace--running-process
                             (process-live-p eshell-carapace--running-process))
                    (insert "   ⚠ Process is hanging! Run M-x eshell-carapace-kill-process\n"))

                  ;; Configuration
                  (insert (format "\n8. Configuration:\n"))
                  (insert (format "   - Timeout: %s seconds\n" eshell-carapace-timeout))
                  (insert (format "   - Debug mode: %s\n" (if eshell-carapace-debug "enabled" "disabled")))

                  (buffer-string))))
    (with-current-buffer (get-buffer-create "*eshell-carapace-diagnostics*")
      (erase-buffer)
      (insert output)
      (goto-char (point-min))
      (special-mode)
      (display-buffer (current-buffer)))))

(provide 'eshell-carapace)

;;; eshell-carapace.el ends here
