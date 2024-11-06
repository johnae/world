(use-package tabspaces
  :ensure t
  :after consult
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  ;(tabspaces-session t)
  ;(tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*")
  :config
  (tabspaces-mode)
  (keymap-global-set "C-x b" 'tabspaces-switch-to-buffer )
  (consult-customize consult--source-buffer :hidden t :default nil)
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                        :predicate #'tabspaces--local-buffer-p
                        :sort 'visibility
                        :as #'buffer-name)))
    "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
