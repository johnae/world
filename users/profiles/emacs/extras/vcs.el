;; with-editor is required by git-commit
(use-package with-editor
  :ensure t)

(use-package git-commit
  :ensure t
  :config
  ;; Recognize JJ temp files as commit messages too
  (setq git-commit-filename-regexp
        (concat git-commit-filename-regexp
                "\\|/editor-.*\\.jjdescription\\'")))

;; Debug code - commented out after fixing project prompt issues
;; (defun my/debug-project-switch (orig-fun &rest args)
;;   (message ">>> project-switch-project called with %S" args)
;;   (backtrace)
;;   (apply orig-fun args))
;; (advice-add 'project-switch-project :around #'my/debug-project-switch)

;; Advise project-current to return nil for commit buffers (preventing prompts)
(defun my/project-current-skip-commit-buffers (orig-fun &rest args)
  "Skip project detection for commit buffers to prevent prompts."
  (if (and buffer-file-name
           (or (string-match-p "COMMIT_EDITMSG\\'" buffer-file-name)
               (string-match-p "MERGE_MSG\\'" buffer-file-name)
               (string-match-p "editor-.*\\.jjdescription\\'" buffer-file-name)))
      ;; Return nil for commit buffers when MAYBE-PROMPT is non-nil
      (if (car args) ; if maybe-prompt is true
          nil      ; return nil to prevent prompt
        (funcall orig-fun nil)) ; otherwise call with nil to prevent prompt
    ;; Normal operation for non-commit buffers
    (apply orig-fun args)))

(advice-add 'project-current :around #'my/project-current-skip-commit-buffers)

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

;; Remove the old hooks that were trying to call git-commit-mode incorrectly
;; git-commit-mode is not a major mode, it's part of the setup process

;; Clean up debug code - can be re-enabled if needed
;; (defun my/debug-mode-changes ()
;;   "Debug mode changes in commit buffers."
;;   (when (and buffer-file-name
;;              (string-match-p "\\(COMMIT_EDITMSG\\|MERGE_MSG\\|editor-.*\\.jjdescription\\)\\'" buffer-file-name))
;;     (message "DEBUG: after-change-major-mode-hook - mode is now %s for %s" major-mode buffer-file-name)))
;; (add-hook 'after-change-major-mode-hook #'my/debug-mode-changes)
