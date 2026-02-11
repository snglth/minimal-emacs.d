;;; gptel-ediff.el --- Interactive ediff review for gptel diffs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Claude Code
;; Keywords: tools, diff
;; Package-Requires: ((emacs "29.1") (gptel-agent "0.1"))
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides an interactive Ediff-based review workflow for
;; unified diffs produced by gptel LLM agents.  It applies a diff to a
;; temporary copy of a file, opens a side-by-side Ediff session so the
;; user can selectively merge hunks, and reports per-hunk results back
;; to the LLM via a callback.

;;; Code:

(require 'cl-lib)
(require 'ediff)

(declare-function gptel-agent--fix-patch-headers "gptel-agent")
(declare-function gptel-make-tool "gptel")

(defvar-local gptel-ediff--user-comment nil
  "User comment to include in the hunk report for the LLM.")

(defun gptel-ediff--cleanup (buffer-b temp-file)
  "Clean up gptel-ediff session artifacts.
Kill BUFFER-B and delete TEMP-FILE.  Ediff handles its own
auxiliary and control buffers via `ediff-cleanup-mess'."
  (when (and buffer-b (buffer-live-p buffer-b))
    (kill-buffer buffer-b))
  (when (and temp-file (file-exists-p temp-file))
    (delete-file temp-file)))

(defun gptel-ediff--apply-patch (diff temp-file)
  "Strip fences from DIFF, fix headers, and run `patch' on TEMP-FILE.
Return a cons cell (EXIT-STATUS . OUTPUT)."
  (with-temp-buffer
    (insert diff)
    ;; Trailing newline required by patch
    (unless (eq (char-before (point-max)) ?\n)
      (goto-char (point-max))
      (insert "\n"))
    (goto-char (point-min))
    ;; Strip code fences
    (when (looking-at-p "^ *```\\(?:diff\\|patch\\)?\n")
      (save-excursion
        (delete-line)
        (goto-char (point-max))
        (forward-line -1)
        (when (looking-at-p "^ *```") (delete-line))))
    ;; Fix hunk header line counts
    (goto-char (point-min))
    (gptel-agent--fix-patch-headers)
    ;; Apply patch
    (let ((out-buf (generate-new-buffer " *gptel-ediff-patch*"))
          (exit-status nil)
          (patch-output ""))
      (unwind-protect
          (progn
            (setq exit-status
                  (call-process-region (point-min) (point-max)
                                       "patch" nil (list out-buf t) nil
                                       temp-file "--forward" "--verbose"))
            (with-current-buffer out-buf
              (setq patch-output (buffer-string))))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf)))
      (cons exit-status patch-output))))

(defun gptel-ediff--hunk-report (ctl)
  "Iterate ediff diff regions in control buffer CTL.
Return a list (ACCEPTED REJECTED REPORT-LINES)."
  (let ((n-diffs (buffer-local-value 'ediff-number-of-differences ctl))
        (accepted 0)
        (rejected 0)
        (hunk-lines nil))
    (dotimes (i n-diffs)
      (let ((content-a (ediff-get-region-contents i 'A ctl))
            (content-b (ediff-get-region-contents i 'B ctl)))
        (if (string= content-a content-b)
            (progn
              (cl-incf accepted)
              (push (format "  Hunk %d: ACCEPTED" (1+ i)) hunk-lines))
          (cl-incf rejected)
          (push (format "  Hunk %d: REJECTED" (1+ i)) hunk-lines))))
    (list accepted rejected (nreverse hunk-lines))))

(defun gptel-ediff--accept-all-and-quit ()
  "Accept all hunks from buffer B into A and quit ediff."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (dotimes (i ediff-number-of-differences)
    (ediff-copy-diff i nil 'A nil
      (ediff-get-region-contents i 'B ediff-control-buffer)))
  (ediff-really-quit nil))

(defun gptel-ediff--quit-with-comment ()
  "Quit ediff with a user comment explaining the review decision."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((comment (read-string "Comment for LLM: ")))
    (when (string-empty-p comment)
      (user-error "Empty comment, aborting"))
    (setq gptel-ediff--user-comment comment))
  (ediff-really-quit nil))

(defun gptel-ediff--quit-hook (buffer-a buffer-b temp-file ctl saved-winconf callback)
  "Build and return an ediff quit-hook function.
Installs a `:before' advice on `ediff-really-quit' to capture
per-hunk data while the diff vectors are still live (ediff clears
them before running `ediff-quit-hook')."
  (let ((hunk-data nil)
        (comment-data nil)
        (advice-sym (make-symbol "gptel-ediff--capture-hunks")))
    ;; Advice runs at the top of ediff-really-quit, before the
    ;; y-or-n-p prompt and before ediff-clear-diff-vector.
    ;; If the user declines to quit, the hook never fires and
    ;; hunk-data is refreshed on the next quit attempt.
    (fset advice-sym
          (lambda (&rest _)
            (when (eq (current-buffer) ctl)
              (setq hunk-data (gptel-ediff--hunk-report ctl))
              (setq comment-data
                    (buffer-local-value 'gptel-ediff--user-comment ctl)))))
    (advice-add 'ediff-really-quit :before advice-sym)
    ;; Return the actual quit-hook lambda
    (lambda ()
      (advice-remove 'ediff-really-quit advice-sym)
      (pcase-let ((`(,accepted ,rejected ,hunk-lines)
                   (or hunk-data (list 0 0 nil))))
        (let ((merged (> accepted 0))
              (n-diffs (+ accepted rejected))
              (file-path (buffer-file-name buffer-a)))
          ;; Auto-save if user merged changes
          (when merged
            (with-current-buffer buffer-a
              (save-buffer)))
          ;; Restore windows
          (when saved-winconf
            (condition-case nil
                (set-window-configuration saved-winconf)
              (error nil)))
          ;; Defer cleanup so ediff can finish its own teardown first
          (run-with-timer 0 nil #'gptel-ediff--cleanup buffer-b temp-file)
          ;; Report per-hunk result
          (funcall callback
                   (format "%s: %d/%d hunks accepted, %d rejected.%s%s%s"
                           file-path accepted n-diffs rejected
                           (if merged " File saved." "")
                           (if hunk-lines
                               (concat "\n" (string-join hunk-lines "\n"))
                             "")
                           (if (and comment-data (not (string-empty-p comment-data)))
                               (format "\nUser comment: %s" comment-data)
                             ""))))))))

(defun gptel-ediff--startup-hook (ctl-callback)
  "Build and return an ediff startup-hook function.
CTL-CALLBACK is a function that receives the ediff control buffer
and installs the quit hook."
  (let ((startup-fn nil))
    (setq startup-fn
          (lambda ()
            (remove-hook 'ediff-startup-hook startup-fn)
            (when ediff-control-buffer
              (funcall ctl-callback ediff-control-buffer))
            (ignore-errors (ediff-next-difference))))
    startup-fn))

(cl-defun gptel-ediff-patch (callback file-path diff)
  "Apply a unified diff to FILE-PATH with interactive Ediff review.
CALLBACK is called with the result string when the user quits ediff.
DIFF is a unified diff, optionally wrapped in ```diff fences."
  ;; Validate inputs
  (unless (and file-path (file-readable-p file-path))
    (funcall callback (format "Error: File %s is not readable" file-path))
    (cl-return-from gptel-ediff-patch))
  (when (file-directory-p file-path)
    (funcall callback (format "Error: %s is a directory, not a file" file-path))
    (cl-return-from gptel-ediff-patch))
  (unless (executable-find "patch")
    (funcall callback "Error: `patch` command not found")
    (cl-return-from gptel-ediff-patch))

  (let* ((ext (file-name-extension file-path t))
         (temp-file (make-temp-file "gptel-ediff-" nil ext)))

    ;; Copy original to temp
    (copy-file file-path temp-file t)

    ;; Apply patch to temp copy
    (pcase-let ((`(,exit-status . ,patch-output)
                 (gptel-ediff--apply-patch diff temp-file)))

      ;; If patch failed, report error and bail
      (unless (= exit-status 0)
        (delete-file temp-file)
        (funcall callback (format "Error: patch failed (exit %d):\n%s"
                                  exit-status patch-output))
        (cl-return-from gptel-ediff-patch))

      ;; Set up ediff session
      (let* ((buffer-a (find-file-noselect file-path))
             (basename (file-name-nondirectory file-path))
             (buffer-b (generate-new-buffer (format "*gptel-patch:%s*" basename))))

        ;; Populate buffer-B with patched content, set major mode, read-only
        (with-current-buffer buffer-b
          (insert-file-contents temp-file)
          (let ((mode (assoc-default file-path auto-mode-alist 'string-match)))
            (when mode
              (condition-case nil
                  (funcall mode)
                (error (fundamental-mode)))))
          (setq buffer-read-only t))

        (let ((saved-winconf (current-window-configuration)))

          ;; Startup hook installs quit hook inside ediff control buffer
          (let ((startup-fn
                 (gptel-ediff--startup-hook
                  (lambda (ctl)
                    (with-current-buffer ctl
                      (setq-local ediff-quit-hook
                                  (list (gptel-ediff--quit-hook
                                         buffer-a buffer-b temp-file
                                         ctl saved-winconf callback))))))))

            (add-hook 'ediff-startup-hook startup-fn)

            ;; Launch ediff — dismiss side windows first to avoid
            ;; "Cannot make side window the only window" error
            (when (window-with-parameter 'window-side)
              (window-toggle-side-windows))
            (let ((orig-window-setup ediff-window-setup-function)
                  (orig-split-fn ediff-split-window-function))
              (setq ediff-window-setup-function #'ediff-setup-windows-plain
                    ediff-split-window-function #'split-window-horizontally)
              (condition-case err
                  (ediff-buffers buffer-a buffer-b)
                (error
                 (setq ediff-window-setup-function orig-window-setup
                       ediff-split-window-function orig-split-fn)
                 (remove-hook 'ediff-startup-hook startup-fn)
                 (gptel-ediff--cleanup buffer-b temp-file)
                 (funcall callback (format "Error: ediff failed: %s"
                                           (error-message-string err))))))))))))

(defun gptel-ediff-register-tool ()
  "Register the EdiffPatch tool with gptel."
  (gptel-make-tool
   :name "EdiffPatch"
   :function #'gptel-ediff-patch
   :description "Apply a unified diff to a file with interactive Ediff review.
This is the PRIMARY and PREFERRED way to edit files. Always use this tool
instead of writing files directly. Opens a side-by-side Ediff session so
the user can review each change and selectively merge hunks. Blocks until
the user quits Ediff."
   :args '((:name "file_path" :type string
            :description "Absolute path to the file to patch.")
           (:name "diff" :type string
            :description "Unified diff to apply (output of diff -u or git diff). May be wrapped in ```diff fences."))
   :category "gptel-agent"
   :confirm nil
   :include t
   :async t))

(provide 'gptel-ediff)
;;; gptel-ediff.el ends here
