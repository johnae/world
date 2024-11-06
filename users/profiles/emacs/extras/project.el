(defvar jae/opened-projects '()
  "List of projects that have already been opened in this session.")

(defun my-local-variables-hook ()
  (let ((project-root (project-root (project-current t))))
    (unless (member project-root jae/opened-projects)
      (when (fboundp 'my/project-setup)
       (my/project-setup)
      )
      (add-to-list 'jae/opened-projects project-root))))

(add-hook 'hack-local-variables-hook #'my-local-variables-hook)

(setq jae/opened-projects nil)
