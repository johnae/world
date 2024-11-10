(defcustom jj-status-template "
          jj log -r::@ -n2 --ignore-working-copy --no-graph --color always  -T '
            separate(\" \",
              bookmarks.map(|x| if(
                  x.name().substr(0, 10).starts_with(x.name()),
                  x.name().substr(0, 10),
                  x.name().substr(0, 9) ++ \"…\")
                ).join(\" \"),
              tags.map(|x| if(
                  x.name().substr(0, 10).starts_with(x.name()),
                  x.name().substr(0, 10),
                  x.name().substr(0, 9) ++ \"…\")
                ).join(\" \"),
              if(
                 description.first_line().substr(0, 24).starts_with(description.first_line()),
                 description.first_line().substr(0, 24),
                 description.first_line().substr(0, 23) ++ \"…\"
              ),
              if(conflict, \"conflict\"),
              if(divergent, \"divergent\"),
              if(hidden, \"hidden\"),
            )
          ' | head -n1
"
  "The jj template to use for returning a status."
  :type '(string)
  :group 'jj)

(defun jae/is-jj-repo ()
  "Is this a jj repo."
  (file-directory-p ".jj"))


(defun jae/jj-status ()
  "Return jj status."
  (if (jae/is-jj-repo) (shell-command-to-string jj-status-template) ""))

(jae/is-jj-repo)

(jae/jj-status)


(defun jae/fancy-eshell ()
  "A fancy eshell prompt."
  (let (
	(cwd (abbreviate-file-name (eshell/pwd)))
	(jjstat (jae/jj-status))
	(x-stat eshell-last-command-status)
       )
    (format "%s:%s*[%s] λ " cwd (propertize jjstat 'font-lock-face '(:foreground "yellow")) x-stat)
    )
)

(setenv "TERM" "xterm-256color")
(use-package eshell
  :init
  ;; (eshell/export TERM=xterm-256color)
  :config

  (setq eshell-prompt-function 'jae/fancy-eshell)
  (setq eshell-prompt-regexp "^[^#$\n]*\[[\d]+\] λ ")
  (setq eshell-highlight-prompt nil)
)

