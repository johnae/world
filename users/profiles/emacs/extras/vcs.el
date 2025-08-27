(use-package git-commit
  :ensure t
  :config
  ;; Recognize JJ temp files as commit messages too
  (setq git-commit-filename-regexp
        (concat git-commit-filename-regexp
                "\\|/editor-.*\\.jjdescription\\'"))
  
  ;; Explicitly add file associations for commit messages
  (add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . git-commit-mode))
  (add-to-list 'auto-mode-alist '("/MERGE_MSG\\'" . git-commit-mode))
  (add-to-list 'auto-mode-alist '("/editor-.*\\.jjdescription\\'" . git-commit-mode)))

;; Debug code - commented out after fixing project prompt issues
;; (defun my/debug-project-switch (orig-fun &rest args)
;;   (message ">>> project-switch-project called with %S" args)
;;   (backtrace)
;;   (apply orig-fun args))
;; (advice-add 'project-switch-project :around #'my/debug-project-switch)

(defun my/project-ignore-commit-buffers (dir)
  "Force JJ/Git commit buffers into a safe dummy project."
  (let ((fname (or (buffer-file-name (buffer-base-buffer)) "")))
    (when (or (string-match-p "COMMIT_EDITMSG\\'" fname)
              (string-match-p "/editor-.*\\.jjdescription\\'" fname))
      ;; A fake 'vc project rooted in /tmp that never prompts
      (cons 'vc "/tmp"))))

;; Add with negative priority to run earlier than other project finders
(add-hook 'project-find-functions #'my/project-ignore-commit-buffers -90)

(defun my/setup-commit-buffer-keys ()
  "Setup keybindings for both git and jj commit buffers."
  ;; Ensure with-editor-mode is active
  (when (fboundp 'with-editor-mode)
    (with-editor-mode 1))
  ;; Setup standard commit keybindings
  (local-set-key (kbd "C-c C-c") #'server-edit)
  (local-set-key (kbd "C-c C-k")
                 (lambda ()
                   (interactive)
                   (set-buffer-modified-p nil)
                   (server-edit-abort))))

;; Hook this to git-commit-mode, which now activates for both git and jj
(add-hook 'git-commit-setup-hook #'my/setup-commit-buffer-keys)
