;;; usr-terminal.el --- Terminal emulator configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal emulator setup:
;; - Eat (Emulate A Terminal)
;; - MisTTY (better shell integration)
;; - Eshell integration

;;; Code:

;; Eshell config (deferred until eshell actually loads)
(with-eval-after-load 'eshell
 (setq eshell-prompt-function
  (lambda nil
   (concat
    (propertize (eshell/pwd) 'face '(:foreground "#8787af"))
    (propertize " " 'face nil)
    (propertize "λ" 'face '(:foreground "#8787af"))
    (propertize " " 'face nil)))))

;; Eat terminal emulator (Emulate A Terminal)
(use-package eat
 :ensure t
 :commands (eat eat-other-window eat-project)
 :hook
 (eat-mode . (lambda ()
              (setq-local scroll-margin 0)
              (setq-local cursor-type 'box)
              (blink-cursor-mode -1)))
 (eshell-load . eat-eshell-mode)
 (eshell-load . eat-eshell-visual-command-mode)
 :custom
 (eat-kill-buffer-on-exit t)
 (eat-enable-blinking-text nil)
 :config
 ;; Display at bottom, always in new window
 (add-to-list 'display-buffer-alist
  '("\\*eat.*\\*"
    (display-buffer-pop-up-window display-buffer-at-bottom)
    (window-height . 0.3)
    (side . bottom)))
 (add-to-list 'display-buffer-alist
  '("\\*eshell.*\\*"
    (display-buffer-pop-up-window display-buffer-at-bottom)
    (window-height . 0.3)
    (side . bottom)))
 (add-to-list 'display-buffer-alist
  '("\\*mistty.*\\*"
    (display-buffer-pop-up-window display-buffer-at-bottom)
    (window-height . 0.3)
    (side . bottom)))
 (add-to-list 'display-buffer-alist
  '("\\*Python\\*"
    (display-buffer-pop-up-window display-buffer-at-bottom)
    (window-height . 0.3)
    (side . bottom)))
 ;; Evil integration
 (add-hook 'eat-mode-hook #'evil-emacs-state))

;; Better shells integration
(use-package mistty
  :bind (("C-c s" . mistty)))

;; MisTTY Eat backend (optional performance enhancement)
;; Uncomment to enable Eat terminal emulator backend for MisTTY
;; Benefits: 3x faster, SIXEL graphics, better mouse support
(use-package mistty-eat
  :load-path "lisp"
  :after mistty
  :custom
  (mistty-eat-backend-enabled t))

(provide 'usr-terminal)
;;; usr-terminal.el ends here
