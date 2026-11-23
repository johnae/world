(use-package catppuccin-theme
  :ensure t
  :demand t  ; Load immediately, not deferred
  :init
  (setq catppuccin-flavor 'frappe)
  :config
  (load-theme 'catppuccin :no-confirm))
