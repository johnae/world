(setq ollama-buddy-host "ollama.9000.dev")
(use-package ollama-buddy
  :ensure t
  :bind ("C-c o" . ollama-buddy-menu)
  :custom
  (ollama-buddy-default-model "mistral-small:24b")
  (ollama-buddy-host "ollama.9000.dev")
  (ollama-buddy-port 11434))
