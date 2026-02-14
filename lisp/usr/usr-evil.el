;;; usr-evil.el --- Vim emulation configuration -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Modal editing with Evil:
;; - Evil (Vim emulation)
;; - Evil-collection (Evil bindings for built-in modes)
;; - Transpose-frame (window manipulation)

;;; Code:

;; Vim emulation
(use-package evil
 :ensure t
 :commands (evil-mode evil-define-key)
 :hook (after-init . evil-mode)
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 (setq evil-undo-system 'undo-fu)
 :custom
 ;; Make :s in visual mode operate only on the actual visual selection
 ;; (character or block), instead of the full lines covered by the selection
 (evil-ex-visual-char-range t)
 ;; Use Vim-style regular expressions in search and substitute commands,
 ;; allowing features like \v (very magic), \zs, and \ze for precise matches
 (evil-ex-search-vim-style-regexp t)
 ;; Enable automatic horizontal split below
 (evil-split-window-below t)
 ;; Enable automatic vertical split to the right
 (evil-vsplit-window-right t)
 ;; Disable echoing Evil state to avoid replacing eldoc
 (evil-echo-state nil)
 ;; Do not move cursor back when exiting insert state
 (evil-move-cursor-back nil)
 ;; Make `v$` exclude the final newline
 (evil-v$-excludes-newline t)
 ;; Allow C-h to delete in insert state
 (evil-want-C-h-delete t)
 ;; Enable C-u to delete back to indentation in insert state
 (evil-want-C-u-delete t)
 ;; Enable fine-grained undo behavior
 (evil-want-fine-undo t)
 ;; Allow moving cursor beyond end-of-line in visual block mode
 (evil-move-beyond-eol t)
 ;; Disable wrapping of search around buffer
 (evil-search-wrap nil)
 ;; Whether Y yanks to the end of the line
 (evil-want-Y-yank-to-eol t)
 :config
 ;; Leader key setup (SPC in normal/visual mode)
 (define-key evil-normal-state-map (kbd "SPC") 'transient-leader)
 (define-key evil-visual-state-map (kbd "SPC") 'transient-leader)
 ;; Ensure leader works in comint-based modes (ielm, shell, etc.)
 (evil-define-key 'normal comint-mode-map (kbd "SPC") 'transient-leader)
 (evil-define-key 'insert comint-mode-map (kbd "C-c SPC") 'transient-leader)
 ;; Vinegar-style: `-` opens dired in parent directory
 (define-key evil-normal-state-map (kbd "-") 'dired-jump))

(use-package evil-collection
 :after evil
 :ensure t
 :diminish evil-collection-unimpaired-mode
 :init
 (setq evil-collection-setup-minibuffer t)
 :config
 (evil-collection-init))

;; Window transpose/rotate operations
(use-package transpose-frame
 :ensure t
 :defer t)

;; Window layout rotation (cycle layouts + rotate buffers)
(use-package rotate
 :ensure t
 :defer t)

(provide 'usr-evil)
;;; usr-evil.el ends here
