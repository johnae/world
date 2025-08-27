(use-package monet
  ;; Don't :ensure because it's provided via Nix melpaBuild
  :defer t)

(use-package claude-code
  ;; Don't :ensure because it's provided via Nix melpaBuild
  :defer t  ; Load when actually needed
  :init
  ;; Configure claude-code to use vterm instead of eat
  (setq claude-code-terminal-backend 'vterm)
  ;; Set these before package loads
  (setq monet-diff-tool #'monet-ediff-tool)
  (setq monet-diff-cleanup-tool #'monet-ediff-cleanup-tool)
  :config
  (require 'monet)  ; Ensure monet is loaded before using it
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))
