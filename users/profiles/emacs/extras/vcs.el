(use-package git-commit
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist
             '("COMMIT_EDITMSG\\'" . git-commit-mode))

(add-to-list 'auto-mode-alist
             '("/editor-.*\\.jjdescription\\'" . git-commit-mode))

(defun my/project-try-commit (dir)
  "If DIR is a Git or Jujutsu commit message, mark it as no-project."
  (let ((fname (buffer-file-name (buffer-base-buffer))))
    (when (and fname
               (string-match-p "\\(COMMIT_EDITMSG\\|\\.jjdescription\\)\\'" fname))
      ;; Return a dummy project cons cell with custom type 'no-project
      (cons 'no-project (file-name-directory fname)))))

(add-hook 'project-find-functions #'my/project-try-commit)  ;; Prepend our detector

(cl-defmethod project-root ((project (head no-project)))
  "Return the root directory of a no-project PROJECT."
  (cdr project))

(cl-defmethod project-files ((project (head no-project)) &optional _dirs)
  "No files in a no-project project."
  ;; Return an empty list to avoid triggering any ‘find’ commands
  '())
