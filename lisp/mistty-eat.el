;;; mistty-eat.el --- Eat terminal backend for MisTTY -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Claude Code
;; Keywords: terminals, processes
;; Package-Requires: ((emacs "29.1") (mistty "1.0") (eat "0.9"))
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides an Eat terminal emulator backend for MisTTY,
;; implemented as a non-invasive extension using Emacs advice system.

;; Overview:
;;
;; MisTTY is a shell interaction mode that brings Emacs-style line editing
;; to terminal shells.  By default, it uses term.el for terminal emulation.
;; This package provides an alternative Eat backend that offers:
;;
;; - 3x faster terminal emulation compared to term.el
;; - SIXEL graphics support for inline images
;; - Better mouse handling (click, drag, modifiers)
;; - Modern, actively maintained codebase
;;
;; All MisTTY features are preserved:
;;
;; - Line editing with Emacs keybindings
;; - OSC handler system (directory tracking, etc.)
;; - Async process queue
;; - Sync markers for buffer coordination
;; - Prompt detection and tracking
;; - Fullscreen application support (vim, htop, etc.)

;; Usage:
;;
;; The following interactive commands are available:
;;
;; `mistty-eat-enable-backend'   - Switch to Eat backend
;; `mistty-eat-disable-backend'  - Switch back to term.el
;; `mistty-eat-toggle-backend'   - Toggle between backends
;; `mistty-eat-backend-status'   - Show current backend status
;; `mistty-eat-uninstall'        - Remove all advice (complete rollback)
;;
;; Note: Backend changes only affect new MisTTY buffers.  Existing buffers
;; continue using their original backend.

;; Architecture:
;;
;; This extension uses `advice-add' to non-invasively hook into MisTTY:
;;
;; - `mistty--create-term' is advised to create Eat terminals
;; - `mistty--emulate-terminal' is advised to route output through Eat
;; - `mistty--attach' is advised to adapt sync markers for Eat
;;
;; When `mistty-eat-backend-enabled' is non-nil, these advices redirect to
;; Eat-based implementations.  When nil, the original term.el behavior is
;; used.  This design ensures zero modifications to MisTTY package files,
;; easy rollback, and compatibility with MisTTY package updates.

;; Customization:
;;
;; `mistty-eat-backend-enabled' - Enable Eat backend (default: nil)
;; `mistty-eat-term-name' - Terminal name for TERM variable
;;                          (default: "xterm-256color")

;; Evil Mode Integration:
;;
;; When Evil mode is active, mistty-eat automatically switches Evil states
;; to provide optimal behavior for different contexts:
;;
;; - At the prompt: Evil normal state (vim keybindings for navigation)
;; - Fullscreen apps (vim, htop, less): Evil emacs state (all keys pass through)
;; - Exiting fullscreen: Returns to Evil normal state
;;
;; Additionally, a passthrough mode is enabled in fullscreen apps to ensure
;; F-keys (F1-F12) send proper escape sequences to the terminal instead of
;; being intercepted by global Emacs keybindings.  This allows programs like
;; htop (F1-F10), midnight commander (F1-F10), and others to receive their
;; expected function key inputs.
;;
;; The integration is automatic and requires no configuration.

;; Troubleshooting:
;;
;; Problem: MisTTY still uses term.el after enabling Eat backend
;; Solution: Close existing MisTTY buffers and create new ones.  Backend
;;           switching only affects new buffers.
;;
;; Problem: Getting "Eat is not available" error
;; Solution: Install the eat package
;;
;; Problem: Want to test both backends
;; Solution: Use M-x mistty-eat-toggle-backend before creating MisTTY
;;           buffers to switch between backends.
;;
;; Problem: Performance issues or strange behavior
;; Solution: Try M-x mistty-eat-disable-backend to switch back to term.el.

;; Verification:
;;
;; After setup, verify the backend is active:
;;
;;   M-x mistty-eat-backend-status
;;
;; This should show "MisTTY backend: Eat (Eat available)".
;;
;; Create a new MisTTY buffer and verify Eat is active:
;;
;;   M-x mistty
;;   M-: (boundp 'eat-terminal) RET
;;
;; This should return t.
;;
;; Test features:
;; - Line editing: Edit commands with Emacs keys before sending
;; - Directory tracking: Run `cd /tmp', then check M-: default-directory
;; - Fullscreen apps: Run vim, htop, or less
;; - Performance: Run `cat' on a large file and compare with term.el

;; Known Limitations:
;;
;; - Terminal modes: Eat uses a different mode system than term.el
;;   (semi-char/char/emacs vs char/line).  Most functionality works,
;;   but some edge cases may differ.
;;
;; - Text properties: Eat uses a different line wrapping mechanism than
;;   term.el's `term-line-wrap' property.  This should be transparent
;;   but may affect some advanced use cases.

;; Reporting Issues:
;;
;; This is a user extension, not part of official MisTTY.  Bug reports
;; and improvements are welcome via:
;;
;; - MisTTY issues: https://github.com/szermatt/mistty
;; - Eat issues: https://codeberg.org/akib/emacs-eat
;;
;; For issues specific to this integration, please include:
;;
;; - Emacs version (M-x emacs-version)
;; - MisTTY version (M-x list-packages, find mistty)
;; - Eat version (M-x list-packages, find eat)
;; - Backend status (M-x mistty-eat-backend-status)
;; - Steps to reproduce the issue

;;; Code:

(require 'mistty)
(require 'eat nil t)  ; Optional dependency

;;; Configuration

(defgroup mistty-eat nil
  "Eat terminal backend for MisTTY."
  :group 'mistty
  :prefix "mistty-eat-")

(defcustom mistty-eat-backend-enabled nil
  "When non-nil, use Eat as MisTTY's terminal emulator backend.
When nil, use the default term.el backend.

This variable should be set before loading MisTTY or creating
MisTTY buffers. To switch backends in existing buffers, use
`mistty-eat-enable-backend' or `mistty-eat-disable-backend'."
  :type 'boolean
  :group 'mistty-eat)

(defcustom mistty-eat-term-name "xterm-256color"
  "Terminal name to use for TERM environment variable.
Common values:
  - \"xterm-256color\" (default, best compatibility)
  - \"xterm\" (basic compatibility)
  - (eat-term-name) for Eat's native terminal type

Note: Using Eat's native terminal type (eat-truecolor, etc.)
requires the terminfo database to be available to shell programs,
which may not work in all environments."
  :type '(choice (const :tag "xterm-256color (recommended)" "xterm-256color")
                 (const :tag "xterm (basic)" "xterm")
                 (const :tag "screen-256color" "screen-256color")
                 (string :tag "Custom TERM value"))
  :group 'mistty-eat)

;;; Feature Detection

(defun mistty-eat--available-p ()
  "Return non-nil if Eat is available and can be used."
  (and (featurep 'eat)
       (fboundp 'eat-term-make)
       (fboundp 'eat-term-process-output)
       (fboundp 'eat-term-resize)
       (fboundp 'eat-term-display-beginning)))

(defun mistty-eat--check-availability ()
  "Signal an error if Eat is not available."
  (unless (mistty-eat--available-p)
    (error "Eat terminal emulator is not available. Please install it first")))

;;; Mode Definition

(define-derived-mode mistty-eat-mode fundamental-mode "MisTTY-Eat"
  "Major mode for MisTTY buffers using Eat terminal backend.
This mode is used internally by mistty-eat and should not be
enabled manually."
  (setq buffer-read-only t))

;;; Terminal Creation

(defun mistty-eat--build-exec-command (program args width height)
  "Build shell command to execute PROGRAM with ARGS.
Includes stty setup for proper terminal configuration.
WIDTH and HEIGHT are terminal dimensions."
  (let* ((erase-char (pcase mistty-del
                       ("\C-h" "^H")
                       ("\d" "^?")
                       (_ "^?")))
         (stty-cmd (format "stty -nl echo rows %d columns %d sane erase %s 2>/dev/null"
                           height width erase-char))
         ;; On remote systems, use login shell via exec -l $SHELL
         ;; This lets SSH's default behavior determine the user's shell
         (exec-cmd (if (file-remote-p default-directory)
                       ;; Remote: use login shell
                       (if args
                           (format "exec -l \"$SHELL\" %s"
                                   (mapconcat #'shell-quote-argument args " "))
                         "exec -l \"$SHELL\"")
                     ;; Local: use specified program
                     (if args
                         (format "exec %s %s"
                                 (shell-quote-argument program)
                                 (mapconcat #'shell-quote-argument args " "))
                       (format "exec %s" (shell-quote-argument program))))))
    (list "/bin/sh" "-c" (format "%s; %s" stty-cmd exec-cmd))))

(defun mistty-eat--create-terminal (name program args local-map width height)
  "Create Eat-based terminal buffer with NAME running PROGRAM with ARGS.
LOCAL-MAP, WIDTH and HEIGHT are MisTTY parameters.

This function creates a terminal buffer using Eat instead of term.el,
while maintaining compatibility with MisTTY's expected buffer structure."
  (mistty-eat--check-availability)

  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      ;; Basic buffer setup (similar to term-mode basics)
      (kill-all-local-variables)
      (setq major-mode 'mistty-eat-mode)
      (setq mode-name "MisTTY-Eat")
      (use-local-map local-map)

      ;; Disable font-lock and jit-lock like MisTTY does
      (font-lock-mode -1)
      (jit-lock-mode nil)

      ;; Set up MisTTY-required local variables
      (setq-local mistty-eat--width width)
      (setq-local mistty-eat--height height)
      (setq-local scroll-margin 0)

      ;; Create prompt cell (MisTTY requirement)
      (setq-local mistty--prompt-cell (mistty--make-prompt-cell))
      (setq-local mistty--scrolline-home (copy-marker (point-min)))
      (setq-local mistty--scrolline-base 0)

      ;; Set term.el compatibility variables for MisTTY
      (setq-local term-width width)
      (setq-local term-height height)
      (setq-local term-char-mode-buffer-read-only t)
      (setq-local term-char-mode-point-at-process-mark t)
      (setq-local term-buffer-maximum-size 0)
      (setq-local term-set-terminal-size t)
      (setq-local term-command-function #'mistty--term-command-hook)

      ;; Create Eat terminal at buffer end
      (goto-char (point-max))
      (let* ((terminal-start (point))
             (eat-term (eat-term-make term-buffer terminal-start)))

        ;; Store terminal object
        (setq-local eat-terminal eat-term)

        ;; Set terminal size
        (eat-term-resize eat-term width height)

        ;; Configure Eat terminal callbacks
        (setf (eat-term-parameter eat-term 'input-function)
              (lambda (_terminal str)
                (when-let ((proc (get-buffer-process term-buffer)))
                  (process-send-string proc str))))

        (setf (eat-term-parameter eat-term 'set-cursor-function)
              (lambda (_terminal state)
                ;; This function is for cursor TYPE/STYLE, not position
                ;; Set cursor type based on state (:block, :bar, :underline, etc.)
                (pcase state
                  (:invisible (setq cursor-type nil))
                  (:block (setq cursor-type 'box))
                  (:bar (setq cursor-type 'bar))
                  (:underline (setq cursor-type 'hbar))
                  (_ (setq cursor-type 'box)))))

        ;; UI command handler for directory tracking, etc.
        (setf (eat-term-parameter eat-term 'ui-command-function)
              #'mistty-eat--ui-command-handler)

        ;; Ring bell handler (suppress or handle bell)
        (setf (eat-term-parameter eat-term 'ring-bell-function)
              (lambda (_terminal)
                (beep)))

        ;; Directory tracking via OSC 7
        (setf (eat-term-parameter eat-term 'set-cwd-function)
              (lambda (_terminal cwd)
                (setq default-directory cwd)))

        ;; Create term-home-marker for MisTTY compatibility
        (setq-local term-home-marker
                    (copy-marker (eat-term-display-beginning eat-term)))

        ;; Store dimensions for term.el compatibility
        (setq-local term-width width)
        (setq-local term-height height)

        ;; Start the process with stty wrapper
        (let* ((exec-command (mistty-eat--build-exec-command program args width height))
               (process-environment
                (let ((env (append
                            (list
                             (format "TERM=%s" mistty-eat-term-name)
                             (format "INSIDE_EMACS=%s,mistty" emacs-version))
                            process-environment)))
                  ;; TERMINFO references a local file. Remove it on remote hosts
                  ;; as it's not useful there. The terminal description is
                  ;; available via TERM/termcap instead.
                  (if (file-remote-p default-directory)
                      (delq nil
                            (mapcar (lambda (var)
                                      (if (string-prefix-p "TERMINFO=" var)
                                          nil
                                        var))
                                    env))
                    ;; Add TERMINFO only for local hosts
                    (cons (format "TERMINFO=%s" eat-term-terminfo-directory)
                          env))))
               (proc (apply #'start-file-process name term-buffer exec-command)))

          ;; Store process and terminal references
          (process-put proc 'eat-terminal eat-term)
          (setf (eat-term-parameter eat-term 'eat--process) proc)

          ;; Set UTF-8 coding for proper emoji/multibyte character support
          ;; Unlike term.el (which uses binary), Eat expects decoded UTF-8
          (set-process-coding-system proc 'utf-8-unix 'utf-8-unix)

          ;; TRAMP compatibility (from MisTTY)
          ;; TRAMP sets adjust-window-size-function to #'ignore, which
          ;; prevents normal terminal resizing from working. Turn it on again.
          (process-put proc 'adjust-window-size-function
                       #'mistty-eat--adjust-process-window-size)

          ;; Set process window size
          (set-process-window-size proc height width)

          ;; Set process mark at end of buffer
          (set-marker (process-mark proc) (point-max))

          ;; Set process filter to MisTTY's accumulator
          (set-process-filter proc (mistty--make-accumulator
                                     #'mistty--emulate-terminal))

          ;; Add after-change hook (MisTTY requirement)
          (add-hook 'after-change-functions #'mistty--after-change-on-term nil t)))

      term-buffer)))

;;; UI Command Bridge

(defun mistty-eat--ui-command-handler (_terminal command)
  "Handle Emacs-specific UI COMMAND from Eat terminal.
This bridges Eat's ui-command-function to MisTTY's term-command-hook."
  (when (boundp 'term-command-function)
    (funcall term-command-function command)))

;;; Process Output Handling

(defun mistty-eat--emulate-terminal (proc str)
  "Process output STR from PROC using Eat terminal emulator.

This function gets the Eat terminal object associated with PROC,
feeds the output to it, and updates the display."
  (when-let* ((buffer (process-buffer proc))
              (eat-term (process-get proc 'eat-terminal)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; Process output with Eat
        (eat-term-process-output eat-term str)

        ;; Redisplay terminal
        (eat-term-redisplay eat-term)

        ;; Get cursor position from Eat terminal and update process mark
        (let ((cursor-pos (eat-term-display-cursor eat-term)))
          (when cursor-pos
            ;; Update process mark to cursor position
            (set-marker (process-mark proc) cursor-pos)

            ;; MisTTY always wants point at process mark (MisTTY requirement)
            (goto-char (process-mark proc))))))))

;;; State Accessor Compatibility Layer

(defun mistty-eat--term-width ()
  "Get terminal width for current backend.
Returns width from Eat terminal or falls back to term.el variable."
  (if (and mistty-eat-backend-enabled eat-terminal)
      (or mistty-eat--width 80)  ; Use stored width or default
    (bound-and-true-p term-width)))

(defun mistty-eat--term-height ()
  "Get terminal height for current backend.
Returns height from Eat terminal or falls back to term.el variable."
  (if (and mistty-eat-backend-enabled eat-terminal)
      (or mistty-eat--height 24)  ; Use stored height or default
    (bound-and-true-p term-height)))

(defun mistty-eat--cursor-column ()
  "Get cursor column for current backend.
Returns 0-based column position."
  (if (and mistty-eat-backend-enabled eat-terminal)
      (- (point) (line-beginning-position))
    (1- (bound-and-true-p term-current-column))))

(defun mistty-eat--home-marker ()
  "Get scrollback start marker for current backend.
For Eat, returns display beginning. For term.el, returns home marker."
  (if (and mistty-eat-backend-enabled eat-terminal)
      (eat-term-display-beginning eat-terminal)
    (bound-and-true-p term-home-marker)))

;;; Sync Marker Adaptation

(defun mistty-eat--setup-sync-marker ()
  "Setup sync marker for Eat backend after MisTTY attachment.
This adapts MisTTY's sync marker to work with Eat's display-beginning
instead of term.el's home-marker."
  (when (and mistty-eat-backend-enabled
             eat-terminal
             (boundp 'mistty-sync-marker))
    ;; Update sync marker to point to Eat's display beginning
    (let ((display-start (eat-term-display-beginning eat-terminal)))
      (set-marker mistty-sync-marker display-start))))

;;; Terminal Resizing

(defun mistty-eat--resize-advice (orig-fun width height)
  "Around-advice for `mistty--set-process-window-size' to resize Eat terminal.
ORIG-FUN is the original function.  WIDTH and HEIGHT are the requested
terminal dimensions.  This runs in the work buffer context, where
`mistty-term-buffer' is available.

When the Eat backend is active, this skips ORIG-FUN entirely to prevent
`term-reset-size' from corrupting the buffer with fake newlines via
`term--unwrap-visible-long-lines'.  Instead it sends SIGWINCH directly,
resizes via Eat, and updates the compatibility variables MisTTY expects."
  (if (and mistty-eat-backend-enabled
           (boundp 'mistty-term-buffer)
           (buffer-live-p mistty-term-buffer)
           (buffer-local-value 'eat-terminal mistty-term-buffer))
      ;; Eat backend active -- bypass term-reset-size completely
      (with-current-buffer mistty-term-buffer
        (let ((w (max width mistty-min-terminal-width))
              (h (max height mistty-min-terminal-height))
              (proc (get-buffer-process (current-buffer))))
          ;; 1. Send SIGWINCH so the shell knows about the new size
          (when proc
            (set-process-window-size proc h w))
          ;; 2. Let Eat reflow its own buffer content
          (let ((inhibit-read-only t))
            (eat-term-resize eat-terminal w h)
            (eat-term-redisplay eat-terminal))
          ;; 3. Update term-width/term-height (MisTTY copies these during refresh)
          (setq-local term-width w)
          (setq-local term-height h)
          ;; 4. Update our tracking variables
          (setq-local mistty-eat--width w)
          (setq-local mistty-eat--height h)
          ;; 5. Re-sync term-home-marker to Eat's display beginning
          (when (and (boundp 'term-home-marker) (markerp term-home-marker))
            (set-marker term-home-marker
                        (eat-term-display-beginning eat-terminal)))
          ;; 6. Update process mark to Eat's cursor position
          (when proc
            (when-let ((cursor-pos (eat-term-display-cursor eat-terminal)))
              (set-marker (process-mark proc) cursor-pos)))))
    ;; Eat not active -- fall through to original (term.el) resize
    (funcall orig-fun width height)))

(defun mistty-eat--adjust-process-window-size (process windows)
  "Resize Eat terminal when the process window dimensions change.
Handles resize during fullscreen mode, when MisTTY's own
window-size-change hook is not active.  Returns (WIDTH . HEIGHT)
for Emacs to call `set-process-window-size'."
  (let ((size (funcall window-adjust-process-window-size-function
                       process windows)))
    (when size
      (let ((w (max (car size) 1))
            (h (max (cdr size) 1))
            (buf (process-buffer process)))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (when (bound-and-true-p eat-terminal)
              (let ((inhibit-read-only t))
                (eat-term-resize eat-terminal w h)
                (eat-term-redisplay eat-terminal))
              (setq-local term-width w)
              (setq-local term-height h)
              (setq-local mistty-eat--width w)
              (setq-local mistty-eat--height h)
              (when (and (boundp 'term-home-marker) (markerp term-home-marker))
                (set-marker term-home-marker
                            (eat-term-display-beginning eat-terminal)))
              (when-let ((cursor-pos (eat-term-display-cursor eat-terminal)))
                (set-marker (process-mark process) cursor-pos)))))))
    size))

;;; Advice Functions (Non-invasive Integration)

(defun mistty-eat--create-term-advice (orig-fun name program args local-map width height)
  "Advice for `mistty--create-term' to use Eat backend when enabled.
ORIG-FUN is the original function.
NAME, PROGRAM, ARGS, LOCAL-MAP, WIDTH, HEIGHT are terminal parameters.

When `mistty-eat-backend-enabled' is t, redirects to Eat-based
terminal creation. Otherwise, calls original term.el implementation."
  (if (and mistty-eat-backend-enabled (mistty-eat--available-p))
      ;; Use Eat backend
      (mistty-eat--create-terminal name program args local-map width height)
    ;; Use original term.el backend
    (funcall orig-fun name program args local-map width height)))

(defun mistty-eat--emulate-terminal-advice (orig-fun proc str)
  "Advice for `mistty--emulate-terminal' to use Eat backend when enabled.
ORIG-FUN is the original function, PROC is the process, STR is the output.

When `mistty-eat-backend-enabled' is t and process has Eat terminal,
uses Eat for output processing. Otherwise, calls original term.el handler."
  (if (and mistty-eat-backend-enabled
           (process-get proc 'eat-terminal))
      ;; Use Eat backend
      (mistty-eat--emulate-terminal proc str)
    ;; Use original term.el backend
    (funcall orig-fun proc str)))

(defun mistty-eat--attach-advice (&rest _args)
  "Advice for `mistty--attach' to setup Eat-specific sync marker.
Called after MisTTY attachment to adapt sync marker for Eat backend."
  (mistty-eat--setup-sync-marker))

;;; Backend Switching Commands

(defun mistty-eat-enable-backend ()
  "Enable Eat terminal backend for MisTTY.
New MisTTY buffers will use Eat instead of term.el.
Existing buffers are not affected."
  (interactive)
  (mistty-eat--check-availability)
  (setq mistty-eat-backend-enabled t)
  (message "MisTTY Eat backend enabled. New MisTTY buffers will use Eat."))

(defun mistty-eat-disable-backend ()
  "Disable Eat terminal backend for MisTTY.
New MisTTY buffers will use term.el (default).
Existing buffers are not affected."
  (interactive)
  (setq mistty-eat-backend-enabled nil)
  (message "MisTTY Eat backend disabled. New MisTTY buffers will use term.el."))

(defun mistty-eat-toggle-backend ()
  "Toggle between Eat and term.el backends for MisTTY.
New MisTTY buffers will use the selected backend.
Existing buffers are not affected."
  (interactive)
  (if mistty-eat-backend-enabled
      (mistty-eat-disable-backend)
    (mistty-eat-enable-backend)))

(defun mistty-eat-backend-status ()
  "Show current MisTTY backend status."
  (interactive)
  (message "MisTTY backend: %s (Eat %s)"
           (if mistty-eat-backend-enabled "Eat" "term.el")
           (if (mistty-eat--available-p) "available" "not available")))

;;; Evil Mode Integration

(defun mistty-eat--send-to-process (sequence)
  "Send escape SEQUENCE to the terminal process."
  (when-let ((proc (get-buffer-process (current-buffer))))
    (process-send-string proc sequence)))

(defun mistty-eat--send-arrow-up ()
  "Send arrow-up to terminal for shell history.
Uses MisTTY's mechanism to properly handle the key at the prompt."
  (interactive)
  (if (fboundp 'mistty-send-key)
      ;; Use MisTTY's send-key for proper handling at prompt
      (mistty-send-key 1 (kbd "<up>"))
    ;; Fallback to direct process send (shouldn't happen)
    (mistty-eat--send-to-process "\e[A")))

(defun mistty-eat--send-arrow-down ()
  "Send arrow-down to terminal for shell history.
Uses MisTTY's mechanism to properly handle the key at the prompt."
  (interactive)
  (if (fboundp 'mistty-send-key)
      ;; Use MisTTY's send-key for proper handling at prompt
      (mistty-send-key 1 (kbd "<down>"))
    ;; Fallback to direct process send (shouldn't happen)
    (mistty-eat--send-to-process "\e[B")))

(defun mistty-eat--send-eof ()
  "Send EOF (C-d) to the terminal.
Useful for exiting shells, ending input, or signaling EOF to programs."
  (interactive)
  (if (fboundp 'mistty-send-key)
      ;; Use MisTTY's send-key for proper handling
      (mistty-send-key 1 (kbd "C-d"))
    ;; Fallback to direct process send
    (mistty-eat--send-to-process "\C-d")))

(defun mistty-eat--send-fkey (sequence)
  "Send F-key SEQUENCE to the terminal process."
  (mistty-eat--send-to-process sequence))

;; Define a keymap that passes F-keys through to the terminal
(defvar mistty-eat-passthrough-map
  (let ((map (make-sparse-keymap)))
    ;; Bind F-keys to send escape sequences directly to terminal process
    (define-key map (kbd "<f1>") (lambda () (interactive) (mistty-eat--send-fkey "\eOP")))
    (define-key map (kbd "<f2>") (lambda () (interactive) (mistty-eat--send-fkey "\eOQ")))
    (define-key map (kbd "<f3>") (lambda () (interactive) (mistty-eat--send-fkey "\eOR")))
    (define-key map (kbd "<f4>") (lambda () (interactive) (mistty-eat--send-fkey "\eOS")))
    (define-key map (kbd "<f5>") (lambda () (interactive) (mistty-eat--send-fkey "\e[15~")))
    (define-key map (kbd "<f6>") (lambda () (interactive) (mistty-eat--send-fkey "\e[17~")))
    (define-key map (kbd "<f7>") (lambda () (interactive) (mistty-eat--send-fkey "\e[18~")))
    (define-key map (kbd "<f8>") (lambda () (interactive) (mistty-eat--send-fkey "\e[19~")))
    (define-key map (kbd "<f9>") (lambda () (interactive) (mistty-eat--send-fkey "\e[20~")))
    (define-key map (kbd "<f10>") (lambda () (interactive) (mistty-eat--send-fkey "\e[21~")))
    (define-key map (kbd "<f11>") (lambda () (interactive) (mistty-eat--send-fkey "\e[23~")))
    (define-key map (kbd "<f12>") (lambda () (interactive) (mistty-eat--send-fkey "\e[24~")))
    map)
  "Keymap that sends F-keys directly to the terminal process for fullscreen apps.")

(define-minor-mode mistty-eat-passthrough-mode
  "Minor mode for fullscreen terminal apps that need F-keys.
Binds F-keys to send escape sequences directly to the terminal process.
Arrow keys are handled separately via Evil insert state keybindings."
  :keymap mistty-eat-passthrough-map
  :lighter " F-pass")

(defun mistty-eat--evil-setup ()
  "Setup initial Evil state and keybindings for MisTTY buffer.
Ensures we start in normal state at the prompt.
Adds arrow key bindings for shell history in insert mode.
Configures cursor shapes: narrow bar in insert mode, box in normal mode.
Adds C-c C-d keybinding to send EOF (C-d) to terminal."
  ;; Add C-c C-d keybinding for sending EOF (works in all states)
  (local-set-key (kbd "C-c C-d") #'mistty-eat--send-eof)

  (when (bound-and-true-p evil-mode)
    ;; Use evil-local-set-key for buffer-local bindings that work immediately
    (evil-local-set-key 'insert (kbd "<up>") #'mistty-eat--send-arrow-up)
    (evil-local-set-key 'insert (kbd "<down>") #'mistty-eat--send-arrow-down)
    ;; Configure cursor shapes
    (setq-local evil-insert-state-cursor 'bar)       ; Narrow vertical bar in insert mode
    (setq-local evil-normal-state-cursor 'box)       ; Box cursor in normal mode
    (setq-local evil-emacs-state-cursor 'box)        ; Box cursor in emacs mode (fullscreen apps)
    ;; Start in normal state for vim keybindings at prompt
    (evil-normal-state)))

(defun mistty-eat--evil-enter-fullscreen ()
  "Switch to Evil emacs state when entering fullscreen mode.
This ensures all keys including F-keys pass through to terminal apps.
Evil emacs state has minimal bindings and lets the terminal handle keys."
  (when (bound-and-true-p evil-mode)
    (with-current-buffer (or mistty-term-buffer (current-buffer))
      (evil-emacs-state)
      ;; Enable passthrough mode to unbind F-keys
      (mistty-eat-passthrough-mode 1))))

(defun mistty-eat--evil-leave-fullscreen ()
  "Return to Evil normal state when leaving fullscreen mode.
This restores vim keybindings for prompt navigation and editing."
  (when (bound-and-true-p evil-mode)
    (with-current-buffer (or mistty-work-buffer (current-buffer))
      ;; Disable passthrough mode
      (when (bound-and-true-p mistty-eat-passthrough-mode)
        (mistty-eat-passthrough-mode -1))
      (evil-normal-state))))

;;; Installation

;;;###autoload
(defun mistty-eat-install ()
  "Install Eat backend advice for MisTTY.
This function adds advice to MisTTY functions to enable Eat backend support.
It is called automatically when this package is loaded."
  (advice-add 'mistty--create-term :around #'mistty-eat--create-term-advice)
  (advice-add 'mistty--emulate-terminal :around #'mistty-eat--emulate-terminal-advice)
  (advice-add 'mistty--attach :after #'mistty-eat--attach-advice)
  (advice-add 'mistty--set-process-window-size :around #'mistty-eat--resize-advice)

  ;; Add Evil mode integration hooks.
  ;; Use with-eval-after-load so hooks are registered regardless of
  ;; whether evil loads before or after mistty-eat.
  (with-eval-after-load 'evil
    ;; Set initial state when MisTTY starts
    (add-hook 'mistty-mode-hook #'mistty-eat--evil-setup)
    ;; Switch to emacs state when entering fullscreen (for F-keys)
    (add-hook 'mistty-entered-fullscreen-hook #'mistty-eat--evil-enter-fullscreen)
    ;; Return to normal state when leaving fullscreen (for vim keys)
    (add-hook 'mistty-left-fullscreen-hook #'mistty-eat--evil-leave-fullscreen)))

;;;###autoload
(defun mistty-eat-uninstall ()
  "Uninstall Eat backend advice from MisTTY.
This function removes all advice added by this package.
After uninstalling, MisTTY will use term.el backend exclusively."
  (interactive)
  (advice-remove 'mistty--create-term #'mistty-eat--create-term-advice)
  (advice-remove 'mistty--emulate-terminal #'mistty-eat--emulate-terminal-advice)
  (advice-remove 'mistty--attach #'mistty-eat--attach-advice)
  (advice-remove 'mistty--set-process-window-size #'mistty-eat--resize-advice)

  ;; Remove Evil mode integration hooks
  (when (featurep 'evil)
    (remove-hook 'mistty-mode-hook #'mistty-eat--evil-setup)
    (remove-hook 'mistty-entered-fullscreen-hook #'mistty-eat--evil-enter-fullscreen)
    (remove-hook 'mistty-left-fullscreen-hook #'mistty-eat--evil-leave-fullscreen))

  (setq mistty-eat-backend-enabled nil)
  (message "MisTTY Eat backend uninstalled."))

;; Install advice when package is loaded
(mistty-eat-install)

(provide 'mistty-eat)

;;; mistty-eat.el ends here
