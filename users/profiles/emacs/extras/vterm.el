;; Vterm configuration
(use-package vterm
  :ensure t
  :config
  ;; Performance optimizations
  (setq vterm-max-scrollback 10000)
  (setq vterm-timer-delay 0.01)
  
  ;; Shell configuration
  (setq vterm-shell (getenv "SHELL"))
  
  ;; Kill buffer when process exits
  (setq vterm-kill-buffer-on-exit t)
  
  ;; Meow integration for vterm
  (with-eval-after-load 'meow
    ;; Start vterm in insert mode
    (add-hook 'vterm-mode-hook
              (lambda ()
                (meow-insert-mode)))
    
    ;; Switch to vterm-copy-mode when entering meow normal mode
    (defun my/meow-vterm-sync-to-normal ()
      "Enter vterm-copy-mode when switching to meow normal mode."
      (when (and (eq major-mode 'vterm-mode)
                 (fboundp 'vterm-copy-mode))
        (vterm-copy-mode 1)))
    
    ;; Exit vterm-copy-mode when entering meow insert mode  
    (defun my/meow-vterm-sync-to-insert ()
      "Exit vterm-copy-mode when switching to meow insert mode."
      (when (and (eq major-mode 'vterm-mode)
                 (bound-and-true-p vterm-copy-mode))
        (vterm-copy-mode -1)))
    
    ;; Hook into meow state changes for current buffer only
    (add-hook 'vterm-mode-hook
              (lambda ()
                (add-hook 'meow-insert-exit-hook #'my/meow-vterm-sync-to-normal nil t)
                (add-hook 'meow-normal-mode-hook #'my/meow-vterm-sync-to-normal nil t)
                (add-hook 'meow-insert-enter-hook #'my/meow-vterm-sync-to-insert nil t))))
  
  ;; Keybindings
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key)  ; Send literal key
         ("C-c C-t" . vterm-copy-mode)   ; Toggle copy mode manually
         :map vterm-copy-mode-map
         ("C-c C-t" . vterm-copy-mode)))
